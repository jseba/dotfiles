local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local active_color = beautiful.temperature_bar_active_color or "#5AA3CC"
local background_color = beautiful.temperature_bar_background_color or "#222222"

local update_interval = 15 -- Seconds

local temperature_bar =
    wibox.widget {
    max_value = 100,
    value = 50,
    forced_height = dpi(10),
    margins = {
        top = dpi(8),
        bottom = dpi(8)
    },
    forced_width = dpi(200),
    shape = gears.shape.rounded_bar,
    bar_shape = gears.shape.rounded_bar,
    color = active_color,
    background_color = background_color,
    border_width = 0,
    border_color = beautiful.border_color,
    widget = wibox.widget.progressbar
}

local function update_widget(temp)
    temperature_bar.value = tonumber(temp)
end

local temperature_script = [[ sh -c "sensors | grep Package | awk '{print $4}' | cut -c 2-3" ]]
awful.widget.watch(
    temperature_script,
    update_interval,
    function(_, stdout)
        update_widget(stdout)
    end
)

return temperature_bar
