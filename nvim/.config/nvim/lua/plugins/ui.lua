return {
    {
        "NTBBloodbath/galaxyline.nvim",
        event = "VeryLazy",
        init = function(plugin)
            local sl = require("config.statusline")
            local gl = require("galaxyline")
            local glcolors = require("galaxyline.themes.colors")["doom-one"]
            local glcond = require("galaxyline.condition")
            local gls = gl.section

            gl.short_line_list = { "Lazy" }

            gls.left[1] = {
                ViMode = {
                    provider = sl.providers.vimode.mode,
                    highlight = { sl.colors["act1"], sl.colors["DarkGoldenrod2"] },
                }
            }

            gls.left[2] = {
                ViModeSeparator = {
                    provider = sl.providers.vimode.separator,
                    highlight = { sl.colors["act1"], sl.colors["DarkGoldenrod2"] },
                }
            }
        end,
    },
}
