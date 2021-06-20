-------------------
---- paq-nvim (MIT)
----
---- from https://github.com/savq/paq-nvim
-------------------

local uv = vim.loop

-- compat (?)
local cmd = vim.api.nvim_command
local vfn = vim.api.nvim_call_function

-- constants
local LOGFILE = vfn('stdpath', {"cache"}) .. "/paq.log"

-- globals
local run_hook                          -- to handle mutual recursion
local paq_dir = vfn('stdpath', {"data"}) .. "/site/pack/paqs"
local packages = {}                     -- table of 'name':{options} pairs
local changes = {}                      -- table of 'name':'changes' pairs
local num_pkgs = 0
local num_to_remove = 0

local ops {
        clone = {ok=0, fail=0, past="cloned"},
        pull = {ok=0, fail=0, past="pulled changes for"},
        remove = {ok=0, fail=0, past="removed"},
}

local function output_result(op, name, ok, is_hook)
        local result, total, msg
        local count = ""
        local failstr = "Failed to "
        local c = ops[op]
        if c then
                result = ok and 'ok' or 'fail'
                c[result] = c[result] + 1
                total = (op == 'remove') and num_to_remove or num_pkgs -- FIXME(?)
                count = string.format("%d/%d", c[result], total)
                msg = ok and c.past or failstr .. op
                if c.ok + c.fail == total then
                        -- no more packages to update
                        c.ok, c.fail = 0, 0
                        cmd("packloadall! | helptags ALL")
                end
        elseif is_hook then
                -- hooks are not counted
                msg = (ok and "ran" or failstr .. "run") .. string.format(" `%s` for", op)
        else
                msg = failstr .. op
        end
        print(string.format("Paq [%s] %s %s", count, msg, name))
end

local function call_proc(process, pkg, args, cwd, is_hook, callback)
        local log, stderr, handle
        log = uv.fs_open(LOGFILE, "a+", 0x1A4)
        stderr = uv.new_pipe(false)
        stderr:open(log)
        handle = uv.spawn(
                process,
                {args=args, cwd=cwd, stdio={nil, nil, stderr}},
                vim.schedule_wrap(function(code)
                        uv.fs_write(log, "\n", -1)      -- space out error messages
                        uv.fs_close(log)
                        stderr:close()
                        handle:close()
                        output_result(args[1] or process, pkg.name, code == 0, is_hook)
                        if type(callback) == 'function' then callback(code) end
                        if not is_hook then run_hook(pkg) end
                end)
        )
end

-- already declared as local
function run_hook(pkg)
        local t, process, args, ok
        t = type(pkg.run)
        if t == 'function' then
                cmd("packadd " .. pkg.name)
                ok = pcall(pkg.run)
                output_result(t, pkg.name, ok, true)
        elseif t == 'string' then
                args = {}
                for word in pkg.run:gmatch("%S+") do
                        table.insert(args, word)
                end
                process = table.remove(args, 1)
                call_proc(process, pkg, args, pkg.dir, true)
        end
end

local function install(pkg)
        local args = {"clone", pkg.url}
        if pkg.exists then
                ops['clone']['ok'] = ops['clone']['ok'] + 1
                return
        elseif pkg.branch then
                vim.list_extend(args, {"-b", pkg.branch})
        end
        vim.list_extend(args, {pkg.dir})
        local callback = function (code)
                if code == 0 then
                        pkg.exists = true
                        changes[pkg.name] = 'installed'
                end
        end
        call_proc("git", pkg, args, nil, nil, callback)
end

local function get_git_hash(dir)
        local function first_line(path)
                local file = uv.fs_open(path, 'r', 0x1A4)
                if file then
                        local line = uv.fs_read(file, 41, -1)   -- XXX: this might fail
                        uv.fs_close(file)
                        return line
                end
        end
        local head_ref = first_line(dir .. "/.git/HEAD")
        if head_ref then
                return first_line(dir .. "/.git/" .. head_ref:gsub("ref: ", ""))
        end
end

local function update(pkg)
        if pkg.exists then
                local hash = get_git_hash(pkg.dir)
                local callback = function (code)
                        if code == 0 and get_git_hash(pkg.dir) ~= hash then
                                changes[pkg.name] = 'updated'
                        end
                end
                call_proc("git", pkg, {"pull"}, pkg.dir, nil, callback)
        end
end

local function iter_dir(fn, dir, args)
        local child, name, t, ok
        local handle = uv.fs_scandir(dir)
        while handle do
                name, t = uv.fs_scandir_next(handle)
                if not name then break end
                child = dir .. "/" .. name
                ok = fn(child, name, t, args)
                if not ok then return end
        end
        return true
end

local function rmdir(child, _, t)
        if t == 'directory' then
                return iter_dir(rmdir, child) and uv.fs_rmdir(child)
        else
                return uv.fs_unlink(child)
        end
end

local function mark_dir(dir, name, _, list)
        local pkg = packages[name]
        if not (pkg and pkg.dir == dir) then
                table.insert(list, {name=name, dir=dir})
        end
        return true
end

local function clean()
        local ok
        local remove_list = {}
        iter_dir(mark_dir, paq_dir .. "start", remove_list)
        iter_dir(mark_dir, paq_dir .. "opt", remove_list)
        num_to_remove = #remove_list
        for _, i in ipairs(remove_list) do
                ok = iter_dir(rmdir, i.dir) and uv.fs_rmdir(i.dir)
                output_result("remove", i.name, ok)
                if ok then changes[i.name] = 'removed' end
        end
end

local function list()
        local installed = vim.tbl_filter
end
