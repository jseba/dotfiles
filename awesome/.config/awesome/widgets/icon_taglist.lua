local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local naughty = require("naughty")
local helpers = require("helpers")

local dpi = xresources.apply_dpi
local capi = { screen = screen, client = client }

-- Assume that all screens have the same number of tags
local s = awful.screen.focused()
local ntags = 5
local tag_icons = {}

for i = 1, ntags do
    table.insert(tag_icons, wibox.widget.imagebox())
    tag_icons[i]:buttons(gears.table.join(
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
    tag_icons[i].forced_width = dpi(25)
    tag_icons[i].align = "center"
    tag_icons[i].valign = "center"
end

local icon_taglist = wibox.widget {
    tag_icons[1],
    tag_icons[2],
    tag_icons[3],
    tag_icons[4],
    tag_icons[5],
    layout = wibox.layout.fixed.horizontal
}

icon_taglist:buttons(gears.table.join(
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
            tag_icons[i].image = beautiful.taglist_icons_focused[i]
        elseif s.tags[i] and s.tags[i].urgent then
            tag_icons[i].image = beautiful.taglist_icons_urgent[i]
        elseif tag_clients and #tag_clients > 0 then
            tag_icons[i].image = beautiful.taglist_icons_occupied[i]
        else
            tag_icons[i].image = beautiful.taglist_icons_empty[i]
        end
    end
end

client.connect_signal("unmanage", function(_) update_widget() end)
client.connect_signal("untagged", function(_) update_widget() end)
client.connect_signal("tagged",   function(_) update_widget() end)
client.connect_signal("screen",   function(_) update_widget() end)

awful.tag.attached_connect_signal(s, "property::selected",  function() update_widget() end)
awful.tag.attached_connect_signal(s, "property::hide",      function() update_widget() end)
awful.tag.attached_connect_signal(s, "property::activated", function() update_widget() end)
awful.tag.attached_connect_signal(s, "property::screen",    function() update_widget() end)
awful.tag.attached_connect_signal(s, "property::index",     function() update_widget() end)
awful.tag.attached_connect_signal(s, "property::urget",     function() update_widget() end)

return icon_taglist
