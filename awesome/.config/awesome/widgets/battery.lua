local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local update_interval = 30 -- Seconds

local battery =
    wibox.widget {
    text = "battery widget",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}

local function update_widget(bat)
    battery.markup = bat .. "%"
end

local bat_script = [[ sh -c 'upower -i $(upower -e | grep BAT) | grep percentage' ]]
awful.widget.watch(
    bat_script,
    update_interval,
    function(widget, stdout)
        local bat = stdout:match(":%s*(.*)..")
        update_widget(bat)
    end
)

return battery
