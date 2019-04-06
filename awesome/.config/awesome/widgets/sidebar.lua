local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local helpers = require("helpers")

local dpi = xresources.apply_dpi
local pad = helpers.pad

local playerctl_button_size = dpi(48)
local icon_size = 36
local progress_bar_width = dpi(215)

local nerd_font_icon_font = "DroidSansMono Nerd Font 18"

local exit_icon = wibox.widget.textbox()
exit_icon.font = nerd_font_icon_font
exit_icon.forced_height = icon_size
exit_icon.markup = helpers.colorize_text(beautiful.nerd_font_power_icon, beautiful.xcolor5)
local exit_text = wibox.widget.textbox("Exit")
exit_text.font = "Noto Sans 14"

local exit =
    wibox.widget {
    exit_icon,
    pad(1),
    exit_text,
    layout = wibox.layout.fixed.horizontal
}
exit:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                exit_screen_show()
                sidebar.visible = false
            end
        )
    )
)

local temperature_icon = wibox.widget.textbox()
temperature_icon.font = nerd_font_icon_font
temperature_icon.forced_height = icon_size
temperature_icon.markup = helpers.colorize_text(beautiful.nerd_font_temperature_icon, beautiful.temperature_bar_active_color)
local temperature_bar = require("widgets.temperature_bar")
temperature_bar.forced_width = progress_bar_width

local temperature =
    wibox.widget {
    nil,
    {
        temperature_icon,
        pad(1),
        temperature_bar,
        pad(1),
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}

local battery_icon = wibox.widget.textbox()
battery_icon.font = nerd_font_icon_font
battery_icon.forced_height = icon_size
battery_icon.markup = helpers.colorize_text(beautiful.nerd_font_battery_icon, beautiful.battery_bar_active_color)
local battery_bar = require("widgets.battery_bar")
battery_bar.forced_width = progress_bar_width

awesome.connect_signal(
    "battery_plugged",
    function(_)
        battery_icon.markup = beautiful.nerd_font_battery_charging_icon
    end
)
awesome.connect_signal(
    "battery_unplugged",
    function(_)
        battery_icon.image = beautiful.nerd_font_battery_icon
    end
)

local battery =
    wibox.widget {
    nil,
    {
        battery_icon,
        pad(1),
        battery_bar,
        pad(1),
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}

local cpu_icon = wibox.widget.textbox()
cpu_icon.font = nerd_font_icon_font
cpu_icon.forced_height = icon_size
cpu_icon.markup = helpers.colorize_text(beautiful.nerd_font_chip_icon, beautiful.cpu_bar_active_color)
local cpu_bar = require("widgets.cpu_bar")
cpu_bar.forced_width = progress_bar_width

local cpu =
    wibox.widget {
    nil,
    {
        cpu_icon,
        pad(1),
        cpu_bar,
        pad(1),
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}
cpu:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.client.run_or_raise(
                    terminal .. " -e htop",
                    function(c)
                        return awful.rules.match(c, {name = "htop"})
                    end
                )
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.client.run_or_raise(
                    "lxtask",
                    function(c)
                        return awful.rules.match(c, {class = "Lxtask"})
                    end
                )
            end
        )
    )
)

local ram_icon = wibox.widget.textbox()
ram_icon.font = nerd_font_icon_font
ram_icon.forced_height = icon_size
ram_icon.markup = helpers.colorize_text(beautiful.nerd_font_memory_icon, beautiful.ram_bar_active_color)
local ram_bar = require("widgets.ram_bar")
ram_bar.forced_width = progress_bar_width

local ram =
    wibox.widget {
    nil,
    {
        ram_icon,
        pad(1),
        ram_bar,
        pad(1),
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}
ram:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.client.run_or_raise(
                    terminal .. " -e htop",
                    function(c)
                        return awful.rules.match(c, {name = "htop"})
                    end
                )
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.client.run_or_raise(
                    "lxtask",
                    function(c)
                        return awful.rules.match(c, {class = "Lxtask"})
                    end
                )
            end
        )
    )
)

local playerctl_toggle_icon = wibox.widget.textbox()
playerctl_toggle_icon.font = "DroidSansMono Nerd Font 24"
playerctl_toggle_icon.forced_height = playerctl_button_size
playerctl_toggle_icon.markup = helpers.colorize_text(beautiful.nerd_font_playerctl_toggle_icon, beautiful.xcolor3)
playerctl_toggle_icon:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("mpc toggle")
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn.with_shell("mpvc toggle")
            end
        )
    )
)

local playerctl_prev_icon = wibox.widget.textbox()
playerctl_prev_icon.font = "DroidSansMono Nerd Font 24"
playerctl_prev_icon.forced_height = playerctl_button_size
playerctl_prev_icon.markup = helpers.colorize_text(beautiful.nerd_font_playerctl_prev_icon, beautiful.xcolor3)
playerctl_prev_icon:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("mpc prev")
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn.with_shell("mpvc prev")
            end
        )
    )
)

local playerctl_next_icon = wibox.widget.textbox()
playerctl_next_icon.font = "DroidSansMono Nerd Font 24"
playerctl_next_icon.forced_height = playerctl_button_size
playerctl_next_icon.markup = helpers.colorize_text(beautiful.nerd_font_playerctl_next_icon, beautiful.xcolor3)
playerctl_next_icon:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("mpc next")
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn.with_shell("mpvc next")
            end
        )
    )
)

local playerctl_buttons =
    wibox.widget {
    nil,
    {
        playerctl_prev_icon,
        pad(1),
        pad(1),
        playerctl_toggle_icon,
        pad(1),
        pad(1),
        playerctl_next_icon,
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}

local time = wibox.widget.textclock("%H %M")
time.align = "center"
time.valign = "center"
time.font = "Noto Sans 48"

local date = wibox.widget.textclock("%A, %B %d")
date.align = "center"
date.valign = "center"
date.font = "Noto Sans Medium 18"

local weather_widget = require("widgets.text_weather")
weather_widget.align = "center"
weather_widget.valign = "center"

local weather_widget_children = weather_widget:get_all_children()
local weather_icon = weather_widget_children[1]
local weather_text = weather_widget_children[2]
weather_icon.font = "DroidSansMono Nerd Font 18"
weather_text.font = "Noto Sans Medium 14"

local weather = wibox.widget {
    nil,
    weather_widget,
    nil,
    layout = wibox.layout.align.horizontal,
    expand = "none"
}

local mpd_song = require("widgets.mpd_song")
local mpd_widget_children = mpd_song:get_all_children()
local mpd_title = mpd_widget_children[1]
local mpd_artist = mpd_widget_children[2]
mpd_title.font = "Noto Sans Medium 14"
mpd_artist.font = "Sans 11"
mpd_title.forced_height = dpi(24)
mpd_artist.forced_height = dpi(16)

mpd_song:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("mpc toggle")
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn("music_terminal")
            end
        ),
        awful.button(
            {},
            4,
            function()
                awful.spawn.with_shell("mpc prev")
            end
        ),
        awful.button(
            {},
            5,
            function()
                awful.spawn.with_shell("mpc next")
            end
        )
    )
)

local disk_space = require("widgets.disk")
disk_space.font = "sans 14"
local disk_icon = wibox.widget.textbox()
disk_icon.font = nerd_font_icon_font
disk_icon.forced_height = icon_size
disk_icon.markup = helpers.colorize_text(beautiful.nerd_font_folder_icon, beautiful.xcolor11)

local disk =
    wibox.widget {
    nil,
    {
        disk_icon,
        pad(1),
        disk_space,
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}
disk:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn(filemanager, {floating = true})
            end
        )
    )
)

local search_icon = wibox.widget.textbox()
search_icon.font = nerd_font_icon_font
search_icon.forced_height = icon_size
search_icon.markup = helpers.colorize_text(beautiful.nerd_font_search_icon, beautiful.xcolor4)
local search_text = wibox.widget.textbox("Search")
search_text.font = "Noto Sans 14"

local search =
    wibox.widget {
    search_icon,
    pad(1),
    search_text,
    layout = wibox.layout.fixed.horizontal
}
search:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("rofi -show combi")
                sidebar.visible = false
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn.with_shell("rofi -show run")
                sidebar.visible = false
            end
        )
    )
)

local volume_icon = wibox.widget.textbox()
volume_icon.font = nerd_font_icon_font
volume_icon.forced_height = icon_size
volume_icon.markup = helpers.colorize_text(beautiful.nerd_font_volume_icon, beautiful.volume_bar_active_color)
local volume_bar = require("widgets.volume_bar")
volume_bar.forced_width = progress_bar_width

local volume =
    wibox.widget {
    nil,
    {
        volume_icon,
        pad(1),
        volume_bar,
        pad(1),
        layout = wibox.layout.fixed.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.horizontal
}

volume:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("volume.sh mute")
            end
        ),
        awful.button(
            {},
            3,
            function()
                local matcher = function(c)
                    return awful.rules.match(c, {class = "Pavucontrol"})
                end
                awful.client.run_or_raise("pavucontrol", matcher)
            end
        ),
        awful.button(
            {},
            4,
            function()
                awful.spawn.with_shell("volume.sh up")
            end
        ),
        awful.button(
            {},
            5,
            function()
                awful.spawn.with_shell("volume.sh down")
            end
        )
    )
)

-- Begin create sidebar

sidebar = wibox({x = 0, y = 0, visible = false, ontop = true, type = "dock"})
sidebar.bg = beautiful.sidebar_bg or beautiful.wibar_bg or "#111111"
sidebar.fg = beautiful.sidebar_fg or beautiful.wibar_fg or "#FFFFFF"
sidebar.opacity = beautiful.sidebar_opacity or 1
sidebar.height = beautiful.sidebar_height or awful.screen.focused().geometry.height
sidebar.width = beautiful.sidebar_width or dpi(300)
sidebar.y = beautiful.sizebar_y or 0
local radius = beautiful.sidebar_border_radius or 0
if beautiful.sidebar_position == "right" then
    sidebar.x = awful.screen.focused().geometry.width - sidebar.width
    sidebar.shape = helpers.prrect(radius, true, false, false, true)
else
    sidebar.x = beautiful.sidebar_x or 0
    sidebar.shape = helpers.prrect(radius, false, true, true, false)
end

sidebar:buttons(
    gears.table.join(
        awful.button(
            {},
            2,
            function()
                sidebar.visible = false
            end
        )
    )
)

if beautiful.hide_sidebar_on_mouse_leave then
    sidebar:connect_signal(
        "mouse::leave",
        function()
            sidebar.visible = false
        end
    )

    -- also show on mouse hover at screen edge
    local sidebar_activator =
        wibox({y = sidebar.y, width = 1, visible = true, ontop = false, opacity = 0, below = true})
    sidebar_activator.height = sidebar.height
    sidebar_activator:connect_signal(
        "mouse::enter",
        function()
            sidebar.visible = true
        end
    )

    if beautiful.sidebar_position == "right" then
        sidebar_activator.x = awful.screen.focused().geometry.width - sidebar_activator.width
    else
        sidebar_activator.x = 0
    end

    sidebar_activator:buttons(
        gears.table.join(
            awful.button(
                {},
                4,
                function()
                    awful.tag.viewprev()
                end
            ),
            awful.button(
                {},
                5,
                function()
                    awful.tag.viewnext()
                end
            )
        )
    )
end

sidebar:setup {
    {
        -- TOP GROUP
        pad(1),
        pad(1),
        time,
        date,
        pad(1),
        -- fancy_date,
        -- pad(1),
        weather,
        pad(1),
        pad(1),
        layout = wibox.layout.fixed.vertical
    },
    {
        -- MIDDLE GROUP
        playerctl_buttons,
        {
            pad(2),
            mpd_song,
            pad(2),
            layout = wibox.layout.align.horizontal
        },
        pad(1),
        pad(1),
        volume,
        cpu,
        ram,
        temperature,
        battery,
        pad(1),
        disk,
        pad(1),
        pad(1),
        layout = wibox.layout.fixed.vertical
    },
    {
        -- BOTTOM GROUP
        {
            nil,
            {
                search,
                pad(5),
                exit,
                pad(2),
                layout = wibox.layout.fixed.horizontal
            },
            nil,
            layout = wibox.layout.align.horizontal,
            expand = "none"
        },
        pad(1),
        layout = wibox.layout.fixed.vertical
    },
    layout = wibox.layout.align.vertical
}
