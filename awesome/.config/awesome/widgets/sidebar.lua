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

local exit_icon = wibox.widget.imagebox(beautiful.poweroff_icon)
exit_icon.resize = true
exit_icon.forced_width = icon_size
exit_icon.forced_height = icon_size
local exit_text = wibox.widget.textbox("Exit")
exit_text.font = "Noto Sans 14"

local exit =
    wibox.widget {
    exit_icon,
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

local temperature_icon = wibox.widget.imagebox(beautiful.temperature_icon)
temperature_icon.resize = true
temperature_icon.forced_width = icon_size
temperature_icon.forced_height = icon_size
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

local battery_icon = wibox.widget.imagebox(beautiful.battery_icon)
battery_icon.resize = true
battery_icon.forced_width = icon_size
battery_icon.forced_height = icon_size
local battery_bar = require("widgets.battery_bar")
battery_bar.forced_width = progress_bar_width

awesome.connect_signal(
    "battery_plugged",
    function(_)
        battery_icon.image = beautiful.battery_charging_icon
    end
)
awesome.connect_signal(
    "battery_unplugged",
    function(_)
        battery_icon.image = beautiful.battery_icon
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

local cpu_icon = wibox.widget.imagebox(beautiful.cpu_icon)
cpu_icon.resize = true
cpu_icon.forced_width = icon_size
cpu_icon.forced_height = icon_size
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

local ram_icon = wibox.widget.imagebox(beautiful.ram_icon)
ram_icon.resize = true
ram_icon.forced_width = icon_size
ram_icon.forced_height = icon_size
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

local playerctl_toggle_icon = wibox.widget.imagebox(beautiful.playerctl_toggle_icon)
playerctl_toggle_icon.resize = true
playerctl_toggle_icon.forced_width = playerctl_button_size
playerctl_toggle_icon.forced_height = playerctl_button_size
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

local playerctl_prev_icon = wibox.widget.imagebox(beautiful.playerctl_prev_icon)
playerctl_prev_icon.resize = true
playerctl_prev_icon.forced_width = playerctl_button_size
playerctl_prev_icon.forced_height = playerctl_button_size
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

local playerctl_next_icon = wibox.widget.imagebox(beautiful.playerctl_next_icon)
playerctl_next_icon.resize = true
playerctl_next_icon.forced_width = playerctl_button_size
playerctl_next_icon.forced_height = playerctl_button_size
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
        playerctl_toggle_icon,
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
local disk_icon = wibox.widget.imagebox(beautiful.files_icon)
disk_icon.resize = true
disk_icon.forced_width = icon_size
disk_icon.forced_height = icon_size

local disk =
    wibox.widget {
    nil,
    {
        disk_icon,
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

local search_icon = wibox.widget.imagebox(beautiful.search_icon)
search_icon.resize = true
search_icon.forced_width = icon_size
search_icon.forced_height = icon_size
local search_text = wibox.widget.textbox("Search")
search_text.font = "Noto Sans 14"

local search =
    wibox.widget {
    search_icon,
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

local volume_icon = wibox.widget.imagebox(beautiful.volume_icon)
volume_icon.resize = true
volume_icon.forced_width = icon_size
volume_icon.forced_height = icon_size
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
        fancy_date,
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
        temperature,
        ram,
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
