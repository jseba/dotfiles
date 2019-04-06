local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local helpers = require("helpers")

local title_color = beautiful.mpd_song_title_color or beautiful.wibar_fg
local artist_color = beautiful.mpd_song_artist_color or beautiful.wibar_fg
local paused_color = beautiful.mpd_song_paused_color or beautiful.normal_fg

local notification_icon = beautiful.music_icon

local mpd_title =
    wibox.widget {
    text = "---------",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}
local mpd_artist =
    wibox.widget {
    text = "---------",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}
local mpd_song =
    wibox.widget {
    mpd_title,
    mpd_artist,
    layout = wibox.layout.fixed.vertical
}

local artist_fg = artist_color
local title_fg = title_color

local last_notification_id
local function send_notification(artist, title)
    notification =
        naughty.notify(
        {
            title = title,
            text = artist,
            icon = notification_icon,
            timeout = 4,
            replaces_id = last_notification_id
        }
    )
    last_notification_id = notification.id
end

local function update_widget()
    awful.spawn.easy_async(
        {"mpc", "-f", "[[%artist%@@%title%@]]"},
        function(stdout)
            local artist
            local title

            if stdout ~= "" then
                artist = stdout:match("(.*)@@")

                title = stdout:match("@@(.*)@")
                title = string.gsub(title, "^%s*(.-)%s*$", "%1")

                -- Fix ampersands
                title = string.gsub(title, "&", "&amp;")
                artist = string.gsub(artist, "&", "&amp;")

                local status = stdout:match("%[(.*)%]")
                status = string.gsub(status, "^%s*(.-)%s*$", "%1")
                if status == "paused" then
                    artist_fg = paused_color
                    title_fg = paused_color
                else
                    if sidebar.visible == false then
                        send_notification(artist, title)
                    end
                end
            else
                title = "<span style='italic'>Not playing</span>"
                artist = ""

                artist_fg = paused_color
                title_fg = paused_color
            end

            mpd_title.markup = helpers.colorize_text(title, title_fg)
            mpd_artist.markup = helpers.colorize_text(artist, artist_fg)
        end
    )
end

update_widget()

local mpd_script = [[ sh -c "mpc idleloop player" ]]
awful.spawn.with_line_callback(
    mpd_script,
    {
        stdout = function(line)
            update_widget()
        end
    }
)

return mpd_song
