local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local update_interval = 120 -- Seconds

local disk =
    wibox.widget {
    text = "free disk space",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}

local function update_widget(disk_space)
    disk.markup = disk_space .. "B free"
end

local disk_script = [[ sh -c "df -k -h /dev/nvme0n1p2 | tail -1 | awk '{print $4}'" ]]
awful.widget.watch(
    disk_script,
    update_interval,
    function(widget, stdout)
        local disk_space = stdout:gsub("^%s*(.-)%s*$", "%1")
        update_widget(disk_space)
    end
)

return disk
