local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local helpers = require("helpers")

local key = ""
local city_id = "5809844" -- TODO
local units = "imperial"
local symbol = "°F"

local update_interval = 1200 -- in seconds

local weather_text =
    wibox.widget {
    text = "Loading...",
    valign = "center",
    widget = wibox.widget.textbox
}

local weather_icon = wibox.widget.textbox()
weather_icon.resize = true
weather_icon.font = beautiful.weather_text_font
weather_icon.markup = helpers.colorize_text(beautiful.whatever_weather_text,
                                            beautiful.xcolor4)
local weather =
    wibox.widget {
    weather_icon,
    weather_text,
    layout = wibox.layout.fixed.horizontal
}

local function update_widget(icon_code, weather_details)
    local markup = beautiful.whatever_weather_text
    if string.find(icon_code, "01d") then
        markup = beautiful.sun_weather_text
    elseif string.find(icon_code, "01n") then
        markup = beautiful.star_weather_text
    elseif string.find(icon_code, "02d") then
        markup = beautiful.dcloud_weather_text
    elseif string.find(icon_code, "02n") then
        markup = beautiful.ncloud_weather_text
    elseif string.find(icon_code, "03") or string.find(icon_code, "04") then
        markup = beautiful.cloud_weather_text
    elseif string.find(icon_code, "09") or string.find(icon_code, "10") then
        markup = beautiful.rain_weather_text
    elseif string.find(icon_code, "11") then
        markup = beautiful.storm_weather_text
    elseif string.find(icon_code, "13") then
        markup = beautiful.snow_weather_text
    elseif string.find(icon_code, "40") or string.find(icon_code, "50") then
        markup = beautiful.mist_weather_text
    end

    weather_icon.markup = helpers.colorize_text(markup.." ", beautiful.xcolor4)

    weather_details = string.gsub(weather_details, "%-0", "0")
    weather_details = weather_details:sub(1, 1):upper() .. weather_details:sub(2)
    weather_text.markup = helpers.colorize_text(weather_details, beautiful.xcolor7)
end

local weather_details_script = [[
    sh -c '
    KEY="]] .. key .. [["
    CITY="]] .. city_id .. [["
    UNITS="]] .. units .. [["
    SYMBOL="]] .. symbol .. [["

    weather=$(curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=$KEY&id=$CITY&units=$UNITS")

    if [ ! -z "$weather" ]; then
        weather_temp=$(echo "$weather" | jq ".main.temp" | cut -d "." -f1)
        weather_icon=$(echo "$weather" | jq -r ".weather[].icon" | head -n1)
        weather_desc=$(echo "$weather" | jq -r ".weather[].description" | head -n1)
        echo "$weather_icon" "$weather_desc" "$weather_temp$SYMBOL"
    else
        echo "...Info unavailable"
    fi
']]

awful.widget.watch(
    weather_details_script,
    update_interval,
    function(widget, stdout)
        local icon_code = string.sub(stdout, 1, 3)
        local weather_details = string.sub(stdout, 5)
        weather_details = string.gsub(weather_details, "^%s*(.-)%s*$", "%1")
        update_widget(icon_code, weather_details)
    end
)

return weather
