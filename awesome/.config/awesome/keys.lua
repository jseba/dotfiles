local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local naughty = require("naughty")
local helpers = require("helpers")

local dpi = xresources.apply_dpi

local keys = {}

-- Modifiers
superkey = "Mod4"
altkey = "Mod1"
ctrlkey = "Control"
shiftkey = "Shift"

-- Mouse bindings on desktop {{{
keys.desktopbuttons =
    gears.table.join(
    -- Left click - clear
    awful.button(
        {},
        1,
        function()
            mymainmenu:hide()
            sidebar.visible = false
            naughty.destroy_all_notifications()

            helpers.single_double_tap(
                function()
                end,
                function()
                    -- If there is no urgent client, go back to the last tag
                    if awful.client.urgent.get() == nil then
                        awful.tag.history.restore()
                    else
                        awful.client.urgent.jumpto()
                    end
                end
            )
        end
    ),
    -- Right click - toggle main menu
    awful.button(
        {},
        3,
        function()
            mymainmenu:toggle()
        end
    ),
    -- Middle click - toggle start screen
    awful.button(
        {},
        2,
        function()
            start_screen_show()
        end
    ),
    -- Scrolling - switch tags
    awful.button({}, 4, awful.tag.viewprev),
    awful.button({}, 5, awful.tag.viewnext),
    -- Side buttons - control volume
    awful.button(
        {},
        8,
        function()
            awful.spawn.with_shell(config_dir .. "/volume.sh down")
        end
    ),
    awful.button(
        {},
        9,
        function()
            awful.spawn.with_shell(config_dir .. "/volume.sh up")
        end
    )
)
-- }}}

-- Global key bindings {{{
keys.globalkeys =
    gears.table.join(
    -- Client {{{
    -- Focus client by direction
    awful.key(
        {superkey},
        "j",
        function()
            awful.client.focus.bydirection("down")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus down", group = "client"}
    ),
    awful.key(
        {superkey},
        "k",
        function()
            awful.client.focus.bydirection("up")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus up", group = "client"}
    ),
    awful.key(
        {superkey},
        "h",
        function()
            awful.client.focus.bydirection("left")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus left", group = "client"}
    ),
    awful.key(
        {superkey},
        "l",
        function()
            awful.client.focus.bydirection("right")
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "focus right", group = "client"}
    ),
    -- Focus client by index
    awful.key(
        {superkey},
        "Tab",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next", group = "client"}
    ),
    awful.key(
        {superkey, shiftkey},
        "Tab",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous", group = "client"}
    ),
    -- Layout manipulation
    awful.key(
        {superkey, shiftkey},
        "j",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move client to edge
                helpers.move_to_edge(client, "down")
            else
                awful.client.swap.bydirection("down", c, nil)
            end
        end,
        {description = "move down", group = "client"}
    ),
    awful.key(
        {superkey, shiftkey},
        "k",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move client to edge
                helpers.move_to_edge(client, "up")
            else
                awful.client.swap.bydirection("up", c, nil)
            end
        end,
        {description = "move up", group = "client"}
    ),
    awful.key(
        {superkey, shiftkey},
        "h",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move client to edge
                helpers.move_to_edge(client, "left")
            else
                awful.client.swap.bydirection("left", c, nil)
            end
        end,
        {description = "move left", group = "client"}
    ),
    awful.key(
        {superkey, shiftkey},
        "l",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move client to edge
                helpers.move_to_edge(client, "right")
            else
                awful.client.swap.bydirection("right", c, nil)
            end
        end,
        {description = "move right", group = "client"}
    ),
    -- Focus urgent client
    awful.key(
        {superkey},
        "u",
        function()
            if awful.client.urgent.get() then
                awful.client.urget.jumpto()
            else
                -- go back to last tag
                awful.tag.history.restore()
            end
        end,
        {description = "jump to urgent", group = "client"}
    ),
    -- }}}

    -- Tag {{{
    -- Adjust master width factor
    awful.key(
        {superkey, ctrlkey},
        "h",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- resize floating client
                client:relative_move(0, 0, dpi(-20), 0)
            else
                awful.tag.incmwfact(-0.05)
            end
        end,
        {description = "decrease master width factor", group = "tag"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "l",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- resize floating client
                client:relative_move(0, 0, dpi(20), 0)
            else
                awful.tag.incmwfact(0.05)
            end
        end,
        {description = "increase master width factor", group = "tag"}
    ),
    -- Adjust number of columns
    awful.key(
        {superkey, ctrlkey, shiftkey},
        "h",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move floating client
                client:relative_move(dpi(-20), 0, 0, 0)
            else
                awful.tag.incncol(1, nil, true)
            end
        end,
        {description = "increase the number of columns", group = "tag"}
    ),
    awful.key(
        {superkey, ctrlkey, shiftkey},
        "l",
        function()
            local client = client.focus
            if client ~= nil and helpers.is_floating(client) then
                -- move floating client
                client:relative_move(dpi(20), 0, 0, 0)
            else
                awful.tag.incncol(-1, nil, true)
            end
        end,
        {description = "decrease the number of columns", group = "tag"}
    ),
    -- Adjust number of master clients
    awful.key(
        {superkey, ctrlkey},
        "j",
        function()
            awful.tag.incmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "tag"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "k",
        function()
            awful.tag.incmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "tag"}
    ),
    -- Adjust gap size for tag
    awful.key(
        {superkey, shiftkey},
        "minus",
        function()
            awful.tag.incgap(5, nil)
        end,
        {description = "increase gap size", group = "tag"}
    ),
    awful.key(
        {superkey},
        "minus",
        function()
            awful.tag.incgap(-5, nil)
        end,
        {description = "decrease gap size", group = "tag"}
    ),
    -- Kill all visible clients for tag
    awful.key(
        {superkey, altkey},
        "q",
        function()
            for _, c in pairs(awful.screen.focused().clients) do
                c:kill()
            end
        end,
        {description = "kill all visible clients", group = "tag"}
    ),
    -- }}}

    -- Screen {{{
    awful.key(
        {superkey},
        "n",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus next", group = "screen"}
    ),
    awful.key(
        {superkey},
        "p",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus previous", group = "screen"}
    ),
    -- }}}

    -- Awesome {{{
    -- Main menu
    awful.key(
        {superkey},
        "w",
        function()
            mymainmenu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),
    -- Exit screen (shutdown, logout, lock, suspend)
    awful.key(
        {superkey},
        "q",
        function()
            exit_screen_show()
        end,
        {description = "show exit screen", group = "awesome"}
    ),
    -- Lock screen
    awful.key(
        {superkey},
        "m",
        function()
            awful.spawn.with_shell("i3lock")
        end,
        {description = "lock screen", group = "awesome"}
    ),
    -- Toggle wibar visibility
    awful.key(
        {superkey},
        "b",
        function()
            local screen = awful.screen.focused()
            screen.mywibox.visible = not screen.mywibox.visible
            if beautiful.wibar_detached then
                screen.useless_wibar.visible = not screen.useless_wibar.visible
            end
        end,
        {description = "toggle wibar visibility", group = "awesome"}
    ),
    -- Toggle sidebar visibility
    awful.key(
        {superkey},
        "grave",
        function()
            sidebar.visible = not sidebar.visible
        end,
        {description = "toggle sidebar visibility", group = "awesome"}
    ),
    -- Toggle tray visibility
    awful.key(
        {superkey},
        "=",
        function()
            local traybox = awful.screen.focused().traybox
            traybox.visible = not traybox.visible
        end,
        {description = "toggle tray visibility", group = "awesome"}
    ),
    -- Show start screen
    awful.key(
        {superkey, ctrlkey},
        "space",
        function()
            start_screen_show()
        end,
        {description = "show start screen", group = "awesome"}
    ),
    -- }}}

    -- Launcher {{{
    -- Spawn terminal
    awful.key(
        {superkey},
        "Return",
        function()
            awful.spawn(terminal)
        end,
        {description = "spawn terminal", group = "launcher"}
    ),
    awful.key(
        {superkey, shiftkey},
        "Return",
        function()
            awful.spawn(floating_terminal, {floating = true})
        end,
        {description = "spawn floating terminal", group = "launcher"}
    ),
    -- Rofi
    awful.key(
        {ctrlkey},
        "space",
        function()
            awful.spawn.with_shell("rofi -show combi")
        end,
        {description = "launcher", group = "launcher"}
    ),
    -- Dismiss notification(s)
    awful.key(
        {superkey},
        "space",
        function()
            naughty.destroy_all_notifications()
        end,
        {description = "dismiss notification(s)", group = "launcher"}
    )
)
-- }}}

-- }}}

-- Tags {{{
-- Assume that all screens have the same number of tags
for i = 1, beautiful.ntags do
    keys.globalkeys =
        gears.table.join(
        keys.globalkeys,
        -- View tag only
        awful.key(
            {superkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                local selected_tag = screen.selected_tag
                -- Tag back and forth:
                -- If you try to focus the same tag that is currently selected, go
                -- back to the previous tag. Useful for quick toggling back and forth.
                if tag then
                    if tag == selected_tag then
                        awful.tag.history.restore()
                    else
                        tag:view_only()
                    end
                end
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display
        awful.key(
            {superkey, ctrlkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            {description = "toggle tag #" .. i, group = "tag"}
        ),
        -- Move client to tag
        awful.key(
            {superkey, shiftkey},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move client to tag #" .. i, group = "tag"}
        ),
        -- Move all visible clients to tag and focus that tag
        awful.key(
            {superkey, altkey},
            "#" .. i + 9,
            function()
                local tag = client.focus.screen.tags[i]
                local clients = awful.screen.focused().clients
                if tag then
                    for _, c in pairs(clients) do
                        c:move_to_tag(tag)
                    end
                    tag:view_only()
                end
            end,
            {description = "move all clients to tag #" .. i, group = "tag"}
        ),
        -- Toggle tag on focused client
        awful.key(
            {superkey, ctrlkey, shiftkey},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            {description = "toggle client on tag #" .. i, group = "tag"}
        )
    )
end

-- }}}

-- Client key bindings {{{
keys.clientkeys =
    gears.table.join(
    -- Move floating client (relative)
    awful.key(
        {superkey, shiftkey},
        "j",
        function(c)
            c:relative_move(0, 40, 0, 0)
        end
    ),
    awful.key(
        {superkey, shiftkey},
        "k",
        function(c)
            c:relative_move(0, -40, 0, 0)
        end
    ),
    awful.key(
        {superkey, shiftkey},
        "h",
        function(c)
            c:relative_move(-40, 40, 0, 0)
        end
    ),
    awful.key(
        {superkey, shiftkey},
        "l",
        function(c)
            c:relative_move(40, 0, 0, 0)
        end
    ),
    -- Center client
    awful.key(
        {superkey},
        "c",
        function(c)
            awful.placement.centered(c, {honor_workarea = true})
        end
    ),
    -- Resize client
    awful.key(
        {superkey, ctrlkey},
        "j",
        function(c)
            if helpers.is_floating(c) then
                c:relative_move(0, 0, 0, dpi(20))
            else
                awful.client.incwfact(0.05)
            end
        end
    ),
    awful.key(
        {superkey, ctrlkey},
        "k",
        function(c)
            if helpers.is_floating(c) then
                c:relative_move(0, 0, 0, dpi(-20))
            else
                awful.client.incwfact(-0.05)
            end
        end
    ),
    -- Relative move
    awful.key(
        {superkey, ctrlkey, shiftkey},
        "j",
        function(c)
            c:relative_move(0, dpi(20), 0, 0)
        end
    ),
    awful.key(
        {superkey, ctrlkey, shiftkey},
        "k",
        function(c)
            c:relative_move(0, dpi(-20), 0, 0)
        end
    ),
    -- Toggle titlebar
    awful.key(
        {superkey},
        "t",
        function(c)
            -- don't  toggle if titlebars are used as borders
            if not beautiful.titlebars_imitate_borders then
                awful.titlebar.toggle(c)
            end
        end,
        {description = "toggle titlebar", group = "client"}
    ),
    -- Toggle titlebar for all visible clients in tag
    awful.key(
        {superkey, shiftkey},
        "t",
        function(c)
            local clients = awful.screen.focused().clients
            for _, c in pairs(clients) do
                -- don't  toggle if titlebars are used as borders
                if not beautiful.titlebars_imitate_borders then
                    awful.titlebar.toggle(c)
                end
            end
        end,
        {description = "toggle all titlebars", group = "client"}
    ),
    -- Toggle fullscreen
    awful.key(
        {superkey},
        "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    -- Resize and set floating (size determined by screen size)
    awful.key(
        {superkey, ctrlkey},
        "f",
        function(c)
            c.width = screen_width * 0.7
            c.height = screen_height * 0.75
            c.floating = true
            awful.placement.centered(c, {honor_workarea = true})
        end,
        {description = "focus mode", group = "client"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "v",
        function(c)
            c.width = screen_width * 0.45
            c.height = screen_height * 0.90
            c.floating = true
            awful.placement.centered(c, {honor_workarea = true})
        end,
        {description = "focus mode", group = "client"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "t",
        function(c)
            c.width = screen_width * 0.3
            c.height = screen_height * 0.35
            c.floating = true
            awful.placement.centered(c, {honor_workarea = true})
        end,
        {description = "tiny mode", group = "client"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "n",
        function(c)
            c.width = screen_width * 0.45
            c.height = screen_height * 0.5
            c.floating = true
            awful.placement.centered(c, {honor_workarea = true})
        end,
        {description = "normal mode", group = "client"}
    ),
    -- Close window
    awful.key(
        {superkey, shiftkey},
        "q",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    -- Toggle floating
    awful.key(
        {superkey, ctrlkey},
        "i",
        function(c)
            local layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            if layout ~= "floating" then
                c.floating = not c.floating
            end
        end,
        {desscription = "toggle floating", group = "client"}
    ),
    -- Move to master slot
    awful.key(
        {superkey, ctrlkey},
        "Return",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = "client"}
    ),
    -- Move to next screen
    awful.key(
        {superkey},
        "o",
        function(c)
            c:move_to_screen(c.screen.index+1)
        end,
        {description = "move to next screen", group = "client"}
    ),
    -- Move to previous screen
    awful.key(
        {superkey, shiftkey},
        "o",
        function(c)
            c:move_to_screen(c.screen.index-1)
        end,
        {description = "move to previous screen", group = "client"}
    ),
    -- Toggle on-top
    awful.key(
        {superkey, ctrlkey},
        "p",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = "client"}
    ),
    -- Toggle sticky (keep on all tags)
    awful.key(
        {superkey, shiftkey},
        "p",
        function(c)
            c.sticky = not c.sticky
        end,
        {description = "toggle sticky", group = "client"}
    ),
    -- Minimize
    awful.key(
        {superkey, shiftkey},
        "y",
        -- The client is currently focused, as minimized clients cannot have focus
        function(c)
            c.minimized = true
        end,
        {description = "minimize", group = "client"}
    ),
    -- Maximize
    awful.key(
        {superkey},
        "y",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = "(un)maximize", group = "client"}
    ),
    awful.key(
        {superkey, ctrlkey},
        "y",
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = "(un)maximize vertically", group = "client"}
    ),
    awful.key(
        {superkey, ctrlkey, shiftkey},
        "y",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = "(un)maximize horizontally", group = "client"}
    )
)
-- }}}

-- Mouse buttons on client {{{
keys.clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            mymainmenu:hide()
            sidebar.visible = false
            client.focus = c
            c:raise()
        end
    ),
    awful.button({superkey}, 1, awful.mouse.client.move),
    awful.button(
        {superkey},
        2,
        function(c)
            c:kill()
        end
    ),
    awful.button(
        {superkey},
        3,
        function(c)
            awful.mouse.client.resize(c)
        end
    )
)
-- }}}

-- Set keys
root.keys(keys.globalkeys)
root.buttons(keys.desktopbuttons)

return keys
