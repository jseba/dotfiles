local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

local active_color = beautiful.cpu_bar_active_color or "#5AA3CC"
local background_color = beautiful.cpu_bar_background_color or "#222222"

local update_interval = 5 -- Seconds

local cpu_bar =
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

local function update_widget(cpu_idle)
    cpu_bar.value = 100 - tonumber(cpu_idle)
end

local cpu_script = [[ sh -c "vmstat 1 2 | tail -1 | awk '{printf \"%d\", $15}'" ]]
awful.widget.watch(
    cpu_script,
    update_interval,
    function(widget, stdout)
        local cpu_idle = 100

        if cpu_idle ~= "" then
            cpu_idle = stdout:gsub("^%s*(.-)%s*$", "%1")
        end

        update_widget(cpu_idle)
    end
)

return cpu_bar
