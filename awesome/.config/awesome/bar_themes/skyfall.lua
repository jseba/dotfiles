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

        -- Create menu button
        local menu_button = wibox.widget.textbox()
        menu_button.font = "DroidSansMono Nerd Font 11"
        menu_button.markup = helpers.colorize_text(beautiful.nerd_font_menu_icon, beautiful.xcolor4)
        menu_button.align = "center"
        menu_button.valign = "center"
        menu_button:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        mymainmenu:toggle()
                    end
                )
            )
        )

        -- Create custom taglist
        local ntags = beautiful.ntags
        local tag_text = {}

        for i = 1, ntags do
            table.insert(tag_text, wibox.widget.textbox(tostring(i)))
            tag_text[i]:buttons(gears.table.join(
                                    -- Left click: tag back and forth
                                    awful.button({}, 1, function()
                                            local current_tag = s.selected_tag
                                            local clicked_tag = s.tags[i]
                                            if clicked_tag == current_tag then
                                                awful.tag.history.restore()
                                            else
                                                clicked_tag:view_only()
                                            end
                                    end),
                                    -- Right click: move focused client to tag
                                    awful.button({}, 3, function()
                                            local clicked_tag = s.tags[i]
                                            if client.focus then
                                                clien.focus:move_to_tag(clicked_tag)
                                            end
                                    end)
                            ))
            -- tag_text[i].markup = helpers.colorize_text(tostring(i), beautiful.taglist_text_color_empty)
            tag_text[i].font = beautiful.taglist_text_font
            tag_text[i].forced_width = dpi(25)
            tag_text[i].align = "center"
            tag_text[i].valign = "center"
        end

        -- FIXME: dynamically add tag widgets
        local text_taglist = wibox.widget {
            tag_text[1],
            tag_text[2],
            tag_text[3],
            tag_text[4],
            tag_text[5],
            tag_text[6],
            tag_text[7],
            tag_text[8],
            tag_text[9],
            tag_text[10],
            layout = wibox.layout.fixed.horizontal
        }

        text_taglist:buttons(gears.table.join(
                                -- Middle click: show clients in current tag
                                awful.button({}, 2, function()
                                        awful.spawn.with_shell("rofi -show windowcd")
                                end),
                                -- Scroll: cycle through tags
                                awful.button({}, 4, function()
                                        awful.tag.viewprev()
                                end),
                                awful.button({}, 5, function()
                                        awful.tag.viewnext()
                                end)
        ))

        local function update_widget()
            for i = 1, ntags do
                local tag_clients
                if s.tags[i] then
                    tag_clients = s.tags[i]:clients()
                end
                if s.tags[i] and s.tags[i].selected then
                    tag_text[i].markup = helpers.colorize_text(beautiful.taglist_text_focused,
                                                            beautiful.taglist_text_color_focused)
                elseif s.tags[i] and s.tags[i].urgent then
                    tag_text[i].markup = helpers.colorize_text(beautiful.taglist_text_urgent,
                                                            beautiful.taglist_text_color_urgent)
                elseif tag_clients and #tag_clients > 0 then
                    tag_text[i].markup = helpers.colorize_text(beautiful.taglist_text_occupied,
                                                            beautiful.taglist_text_color_occupied)
                else
                    tag_text[i].markup = helpers.colorize_text(beautiful.taglist_text_empty,
                                                            beautiful.taglist_text_color_empty)
                end
            end
        end

        client.connect_signal("unmanaged", function(_) update_widget() end)
        client.connect_signal("untagged",  function(_) update_widget() end)
        client.connect_signal("tagged",    function(_) update_widget() end)
        client.connect_signal("screen",    function(_) update_widget() end)

        awful.tag.attached_connect_signal(s, "property::selected",  function() update_widget() end)
        awful.tag.attached_connect_signal(s, "property::hide",      function() update_widget() end)
        awful.tag.attached_connect_signal(s, "property::activated", function() update_widget() end)
        awful.tag.attached_connect_signal(s, "property::screen",    function() update_widget() end)
        awful.tag.attached_connect_signal(s, "property::index",     function() update_widget() end)
        awful.tag.attached_connect_signal(s, "property::urget",     function() update_widget() end)

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
                            client.focus.maximized = not client.focus.maximized
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
                width = beautiful.wibar_width or s.geometry.width,
                height = beautiful.wibar_height or s.geometry.height,
                shape = helpers.rrect(beautiful.wibar_border_radius),
                bg = beautiful.wibar_bg
            }
        )
        s.mywibox:setup {
            {
                pad(1),
                menu_button,
                layout = wibox.layout.fixed.horizontal
            },
            {
                text_taglist,
                { -- Space padding
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
