local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local update_interval = 15 -- Seconds

local temperature =
    wibox.widget {
        text = "?? °C",
        align = "center",
        valign = "center",
        widget = wibox.widget.textbox
}

local function update_widget(temp)
    temperature.marktup = tonumber(temp)
end

local temperature_script = [[ sh -c "sensors | grep Package | awk '{print $4}' | cut -c 2-8" ]]
awful.widget.watch(
    temperature_script,
    update_interval,
    function(_, stdout)
        local temp = stdout:gsub("^%s*(.-)%s*$", "%1")
        update_widget(stdout)
    end
)

return temperature
