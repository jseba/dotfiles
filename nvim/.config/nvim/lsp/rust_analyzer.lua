return {
    cmd = { 'rust_analyzer' },
    filetypes = { 
        'rust',
    },
    root_markers = {
        'Cargo.toml',
    },
    before_init = function(init_params, config)
        if config.settings and config.settings['rust_analyzer'] then
            init_params.initializationOptions = config.settings['rust_analyzer']
        end
    end,
    on_attach = function()
        vim.api.nvim_buf_create_user_command(0, 'LspCargoReload', function()
            reload_workspace(0)
        end,
        { desc, 'Reload current Cargo workspace' })
    end,
    capabilities = {
        experimental = {
            serverStatusNotification = true,
        },
    },
    settings = {
        rust_analyzer = {
            checkOnSave = {
                enabled = true,
                command = 'clippy',
            },
        },
    },
}
