local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local helpers = require("helpers")

local dpi = xresources.apply_dpi
local pad = helpers.pad

-- Widgets {{{
local taglist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {modkey},
        2,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    ),
    awful.button(
        {},
        3,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button(
        {},
        4,
        function(t)
            awful.tag.viewprev(t.screen)
        end
    ),
    awful.button(
        {},
        5,
        function(t)
            awful.tag.viewnext(t.screen)
        end
    )
)

local tasklist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            if c == client.focus then
                c.minimized = true
            else
                c.minimized = false
                if not c:isvisible() and c.first_tag then
                    c.first_tag:view_only()
                end
                client.focus = c
                c:raise()
            end
        end
    ),
    awful.button(
        {},
        2,
        function(c)
            c:kill()
        end
    ),
    awful.button(
        {},
        3,
        function(c)
            c.minimized = true
        end
    ),
    awful.button(
        {},
        4,
        function()
            awful.client.focus.byidx(-1)
        end
    ),
    awful.button(
        {},
        5,
        function()
            awful.client.focus.byidx(1)
        end
    )
)
-- }}}

awful.screen.connect_for_each_screen(
    function(s)
        -- Create a system tray widget
        s.systray = wibox.widget.systray()

        -- Create a wibox that will only show the tray
        -- Hidden by default; can be toggled with a keybind
        s.traybox =
            wibox(
            {
                visible = false,
                ontop = true,
                shape = helpers.rrect(beautiful.border_radius),
                type = "dock"
            }
        )
        s.traybox.width = dpi(120)
        s.traybox.height = beautiful.wibar_height - beautiful.screen_margin * 4
        s.traybox.x = s.geometry.width - beautiful.screen_margin * 2 - s.traybox.width
        s.traybox.y = s.geometry.height - beautiful.screen_margin * 2
        s.traybox.bg = beautiful.bg_systray
        s.traybox:setup {
            pad(1),
            s.systray,
            pad(1),
            layout = wibox.layout.align.horizontal
        }
        s.traybox:buttons(
            gears.table.join(
                -- Middle click hides the traybox
                awful.button(
                    {},
                    2,
                    function()
                        s.traybox.visible = true
                    end
                )
            )
        )

        -- Hide traybox when mouse leaves
        s.traybox:connect_signal(
            "mouse::leave",
            function()
                s.traybox.visible = false
            end
        )

        -- Create custom taglist
        local text_taglist = require("widgets.text_taglist")

        -- Create a widget that displays window buttons (close, minimize, maximize)
        local close_button = wibox.widget.textbox()
        close_button.font = "DroidSansMono Nerd Font 11"
        close_button.markup = helpers.colorize_text(beautiful.nerd_font_close_icon, beautiful.xcolor5)
        close_button.align = "center"
        close_button.valign = "center"
        close_button:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        if client.focus then
                            client.focus:kill()
                        end
                    end
                )
            )
        )

        local maximize_button = wibox.widget.textbox()
        maximize_button.font = "DroidSansMono Nerd Font 11"
        maximize_button.markup = helpers.colorize_text(beautiful.nerd_font_maximize_icon, beautiful.xcolor5)
        maximize_button.align = "center"
        maximize_button.valign = "center"
        maximize_button:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        if client.focus then
                            client.focus.maximized = not client.focus.maximize
                        end
                    end
                )
            )
        )

        local minimize_button = wibox.widget.textbox()
        minimize_button.font = "DroidSansMono Nerd Font 11"
        minimize_button.markup = helpers.colorize_text(beautiful.nerd_font_minimize_icon, beautiful.xcolor5)
        minimize_button.align = "center"
        minimize_button.valign = "center"
        minimize_button:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        if client.focus then
                            client.focus.minimized = true
                        end
                    end
                )
            )
        )

        local window_buttons =
            wibox.widget {
            minimize_button,
            maximize_button,
            close_button,
            {
                spacing = dpi(6),
                layout = wibox.layout.fixed.horizontal
            },
            spacing = dpi(12),
            layout = wibox.layout.fixed.horizontal
        }
        window_buttons:buttons(
            gears.table.join(
                awful.button(
                    {},
                    2,
                    function()
                        awful.spawn.with_shell("rofi -show windowcd")
                    end
                ),
                awful.button(
                    {},
                    4,
                    function()
                        awful.client.focus.byidx(-1)
                    end
                ),
                awful.button(
                    {},
                    5,
                    function()
                        awful.client.focus.byidx(1)
                    end
                )
            )
        )

        -- Create the wibox
        s.mywibox =
            awful.wibar(
            {
                position = beautiful.wibar_position,
                screen = s,
                width = beautiful.wibar_width,
                height = beautiful.wibar_height,
                shape = helpers.rrect(beautiful.wibar_border_radius),
                bg = beautiful.wibar_bg
            }
        )
        s.mywibox:setup {
            text_taglist,
            {
                {
                    -- Space padding
                    layout = wibox.layout.fixed.horizontal
                },
                spacing = dpi(12),
                layout = wibox.layout.fixed.horizontal
            },
            window_buttons,
            expand = "none",
            layout = wibox.layout.align.horizontal
        }
    end
)

local s = mouse.screen
-- Show traybox when the mouse touches the rightmost edge of the wibar
traybox_activator =
    wibox(
    {
        x = s.geometry.width - 1,
        y = s.geometry.height - beautiful.wibar_height,
        height = beautiful.wibar_height,
        width = 1,
        opacity = 0,
        visible = true,
        bg = beautiful.wibar_bg
    }
)
traybox_activator:connect_signal(
    "mouse::enter",
    function()
        s.traybox.visible = true
    end
)
