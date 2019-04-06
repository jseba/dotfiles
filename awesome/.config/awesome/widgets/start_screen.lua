local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local naughty = require("naughty")
local awful = require("awful")
local keygrabber = require("awful.keygrabber")
local helpers = require("helpers")

local dpi = xresources.apply_dpi
local pad = helpers.pad

local box_radius = beautiful.start_screen_box_border_radius or dpi(12)
local box_gap = dpi(6)

local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

local icon_size = dpi(40)

start_screen = wibox({x = 0, y = 0, visible = false, ontop = true, type = "dock", height = screen_height, width = screen_width})
start_screen.fg = beautiful.start_screen_fg or beautiful.exit_screen_fg or beautiful.wibar_fg or "#111111"
start_screen.bg = beautiful.start_screen_bg or beautiful.exit_screen_bg or beautiful.wibar_bg or "#FEFEFE"
start_screen.opacity = beautiful.start_screen_opacity or beautiful.exit_screen_opacity or 1

start_screen:buttons(
    gears.table.join(
        awful.button(
            {},
            2,
            function()
                start_screen_hide()
            end
        )
    )
)

-- Puts a widget inside a box with invisible margins so that multiple boxes are
-- evenly separated; the widget is horizontally and vertically centered in the box.
local function create_boxed_widget(widget, width, height, bg_color)
    local box = wibox.container.background()
    box.bg = bg_color
    box.forced_height = height
    box.forced_width = width
    box.shape = helpers.rrect(box_radius)

    local boxed_widget =
        wibox.widget {
        {
            -- Margins
            {
                -- Background color
                nil,
                {
                    nil,
                    widget,
                    nil,
                    layout = wibox.layout.align.vertical,
                    expand = "none"
                },
                nil,
                layout = wibox.layout.align.horizontal,
                expand = "none"
            },
            widget = box
        },
        margins = box_gap,
        color = "#FF000000",
        widget = wibox.container.margin
    }

    return boxed_widget
end

local user_picture_container = wibox.container.background()
user_picture_container.shape = gears.shape.circle
user_picture_container.forced_height = dpi(130)
user_picture_container.forced_width = dpi(130)
local user_picture =
    wibox.widget {
    wibox.widget.imagebox(os.getenv("HOME") .. "/.config/awesome/profile.png"),
    widget = user_picture_container
}

local username = os.getenv("USER")
local user_text = wibox.widget.textbox(username)
user_text.font = "sans bold 18"
user_text.align = "center"
user_text.valign = "center"

local host_text = wibox.widget.textbox()
awful.spawn.easy_async_with_shell(
    "hostname",
    function(stdout)
        stdout = string.gsub(stdout, "^%s(.-)%s*$", "%1")
        host_text.markup = helpers.colorize_text("@" .. stdout, beautiful.xcolor8)
    end
)
host_text.font = "sans italic 18"
host_text.align = "center"
host_text.valign = "center"

local user_widget =
    wibox.widget {
    user_picture,
    pad(1),
    pad(1),
    pad(1),
    user_text,
    host_text,
    layout = wibox.layout.fixed.vertical
}
local user_box = create_boxed_widget(user_widget, dpi(300), dpi(340), beautiful.xbackground)

local styles = {}
styles.month = {
    padding = 20,
    fg_color = beautiful.xcolor7,
    bg_color = beautiful.xbackground .. "00",
    border_width = 0
}
styles.weekday = {
    fg_color = beautiful.xcolor7,
    bg_color = beautiful.xcolor1 .. "00",
    padding = 3,
    markup = function(t)
        return "<b>" .. t .. "</b>"
    end
}
styles.focus = {
    fg_color = beautiful.xcolor1,
    bg_color = beautiful.xcolor5 .. "00",
    markup = function(t)
        return "<b>" .. t .. "</b>"
    end
}
styles.header = {
    fg_color = beautiful.xcolor7,
    bg_color = beautiful.xcolor1 .. "00",
    markup = function(t)
        return helpers.fontify_text(t, "sans bold 14")
    end
}
styles.normal = {}

local function decorate_cell(widget, flag, date)
    if flag == "month_header" and not styles.month_header then
        flag = "header"
    end
    local props = styles[flag] or {}
    if props.markup and widget.get_text and widget.set_markup then
        widget:set_markup(props.markup(widget:get_text()))
    end

    local default_fg = beautiful.xcolor7
    local default_bg = beautiful.xcolor0 .. "00"

    local cell =
        wibox.widget {
        {
            widget,
            margins = (props.padding or 2) + (props.border_width or 0),
            widget = wibox.container.margin
        },
        shape = props.shape,
        shape_border_color = props.border_color or beautiful.xbackground,
        shape_border_width = props.border_width or 0,
        fg = props.fg_color or default_fg,
        bg = props.bg_color or default_bg,
        widget = wibox.container.background
    }

    return cell
end

local current_date = os.date("*t")
calendar_widget =
    wibox.widget {
    date = current_date,
    font = "Noto Sans 12",
    long_weekdays = false,
    spacing = dpi(3),
    fn_embed = decorate_cell,
    widget = wibox.widget.calendar.month
}

local current_month = current_date.month
calendar_widget:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                calendar_widget.date = os.date("*t")
            end
        ),
        awful.button(
            {},
            4,
            function()
                new_calendar_month = calendar_widget.date.month - 1
                if new_calendar_month == current_month then
                    calendar_widget.date = os.date("*t")
                else
                    calendar_widget.date = {
                        month = new_calendar_month,
                        year = calendar_widget.date.year
                    }
                end
            end
        ),
        awful.button(
            {},
            5,
            function()
                new_calendar_month = calendar_widget.date.month + 1
                if new_calendar_month == current_month then
                    calendar_widget.date = os.date("*t")
                else
                    calendar_widget.date = {
                        month = new_calendar_month,
                        year = calendar_widget.date.year
                    }
                end
            end
        )
    )
)

local calendar_box = create_boxed_widget(calendar_widget, dpi(300), dpi(300), beautiful.xbackground)

local hours = wibox.widget.textclock("%H")
hours.font = "sans bold 30"
hours.align = "center"
hours.valign = "center"
local time_sep = wibox.widget.textbox(":")
time_sep.font = "sans 30"
time_sep.align = "center"
time_sep.valign = "center"
time_sep.markup = helpers.colorize_text(time_sep.text, beautiful.xcolor8)
local minutes = wibox.widget.textclock("%M")
minutes.font = "sans bold 30"
minutes.align = "center"
minutes.valign = "center"

local time =
    wibox.widget {
    hours,
    time_sep,
    minutes,
    layout = wibox.layout.fixed.horizontal
}
local time_box = create_boxed_widget(time, dpi(150), dpi(150), beautiful.xbackground)

local day_of_week = wibox.widget.textclock("%A")
day_of_week.font = "sans italic 20"
day_of_week.fg = beautiful.xcolor0
day_of_week.align = "center"
day_of_week.valign = "center"
local day_of_month = wibox.widget.textclock("%d")
day_of_month.font = "sans italic 20"
day_of_month.fg = beautiful.xcolor0
day_of_month.align = "center"
day_of_month.valign = "center"
day_of_month.markup = helpers.colorize_text(day_of_month.text, beautiful.xcolor1)
day_of_month:connect_signal(
    "widget::redraw_needed",
    function()
        day_of_month.markup = helpers.colorize_text(day_of_month.text, beautiful.xcolor1)
    end
)

local date =
    wibox.widget {
    day_of_week,
    day_of_month,
    layout = wibox.layout.align.vertical
}
local date_box = create_boxed_widget(date, dpi(150), dpi(150), beautiful.xbackground)

local function create_bookmark(name, path)
    local orig_color = beautiful.xcolor1
    local hover_color = beautiful.xcolor9

    local bookmark = wibox.widget.textbox()
    bookmark.font = "Noto Sans bold 16"
    bookmark.markup = helpers.colorize_text(name, orig_color)
    bookmark.align = "center"
    bookmark.valign = "center"

    bookmark:buttons(
        gears.table.join(
            awful.button(
                {},
                1,
                function()
                    awful.spawn.with_shell(filemanager .. "path")
                    start_screen_hide()
                end
            ),
            awful.button(
                {},
                3,
                function()
                    awful.spawn.with_shell(terminal .. " -e 'ranger' " .. path)
                    start_screen_hide()
                end
            )
        )
    )

    bookmark:connect_signal(
        "mouse::enter",
        function()
            bookmark.markup = helpers.colorize_text(name, hover_color)
        end
    )
    bookmark:connect_signal(
        "mouse::leave",
        function()
            bookmark.markup = helpers.colorize_text(name, orig_color)
        end
    )

    helpers.add_clickable_effect(bookmark)

    return bookmark
end

local bookmarks =
    wibox.widget {
    create_bookmark("HOME", "~/"),
    pad(1),
    create_bookmark("DOWNLOADS", "~/"),
    pad(1),
    create_bookmark("MUSIC", "~/"),
    pad(1),
    create_bookmark("PICTURES", "~/"),
    pad(1),
    create_bookmark("SOURCES", "~/"),
    pad(1),
    create_bookmark("WORKSPACE", "~/"),
    layout = wibox.layout.fixed.vertical
}
local bookmarks_box = create_boxed_widget(bookmarks, dpi(200), dpi(300), beautiful.xbackground)

local function create_url(name, path)
    local orig_color = beautiful.xcolor4
    local hover_color = beautiful.xcolor12

    local url = wibox.widget.textbox()
    url.font = "Noto Sans bold 16"
    url.markup = helpers.colorize_text(name, orig_color)
    url.align = "center"
    url.valign = "center"

    url:buttons(
        gears.table.join(
            awful.button(
                {},
                1,
                function()
                    awful.spawn.with_shell(browser .. " " .. path)
                    start_screen_hide()
                end
            ),
            awful.button(
                {},
                3,
                function()
                    awful.spawn.with_shell(browser .. " -new-window " .. path)
                    start_screen_hide()
                end
            )
        )
    )

    url:connect_signal(
        "mouse::enter",
        function()
            url.markup = helpers.colorize_text(name, hover_color)
        end
    )
    url:connect_signal(
        "mouse::leave",
        function()
            url.markup = helpers.colorize_text(name, orig_color)
        end
    )

    helpers.add_clickable_effect(url)

    return url
end

local urls =
    wibox.widget {
    create_url("LOBSTE.RS", "https://lobste.rs"),
    pad(1),
    create_url("REDDIT", "https://reddit.com"),
    pad(1),
    create_url("GITHUB", "https://github.com/jseba"),
    layout = wibox.layout.fixed.vertical
}
local urls_box = create_boxed_widget(urls, dpi(200), dpi(180), beautiful.xbackground)

local brightness_icon = wibox.widget.imagebox(beautiful.redshift_icon)
brightness_icon.resize = true
brightness_icon.forced_width = icon_size
brightness_icon.forced_height = icon_size
local brightness_bar = require("widgets.brightness_bar")
brightness_bar.forced_width = dpi(210)

local brightness =
    wibox.widget {
    brightness_icon,
    pad(1),
    brightness_bar,
    layout = wibox.layout.fixed.horizontal
}

local brightness_box = create_boxed_widget(brightness, dpi(300), dpi(80), beautiful.xbackground)

brightness_box:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell("pkill -USR1 redshift")
            end
        ),
        awful.button(
            {},
            3,
            function()
                awful.spawn.easy_async_with_shell(
                    "xbacklight -set 100",
                    function()
                        awesome.emit_signal("brightness_changed")
                    end
                )
            end
        ),
        awful.button(
            {},
            4,
            function()
                awful.spawn.easy_async_with_shell(
                    "xbacklight -inc 10",
                    function()
                        awesome.emit_signal("brightness_changed")
                    end
                )
            end
        ),
        awful.button(
            {},
            5,
            function()
                awful.spawn.easy_async_with_shell(
                    "xbacklight -dec 10",
                    function()
                        awesome.emit_signal("brightness_changed")
                    end
                )
            end
        )
    )
)

helpers.add_clickable_effect(brightness_box)

local notification_state = wibox.widget.imagebox(beautiful.alarm_icon)
notification_state.resize = true
notification_state.forced_width = icon_size
notification_state.forced_height = icon_size
local function update_notification_state_icon()
    if naughty.is_suspended() then
        notification_state.image = beautiful.alarm_off_icon
    else
        notification_state.image = beautiful.alarm_icon
    end
end
update_notification_state_icon() -- Fire immediately at startup

local notification_state_box = create_boxed_widget(notification_state, dpi(150), dpi(78), beautiful.xbackground)

notification_state_box:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                naughty.toggle()
                update_notification_state_icon()
            end
        )
    )
)

helpers.add_clickable_effect(notification_state_box)

local screenshot = wibox.widget.imagebox(beautiful.screenshot_icon)
screenshot.resize = true
screenshot.forced_width = icon_size
screenshot.forced_height = icon_size

local screenshot_box = create_boxed_widget(screenshot, dpi(150), dpi(78), beautiful.xbackground)

local screeshot_script = os.getenv("HOME") .. "/.config/awesome/screenshot.sh"
screenshot_box:buttons(
    gears.table.join(
        awful.button(
            {},
            1,
            function()
                awful.spawn.with_shell()
            end
        ),
        awful.button(
            {},
            3,
            function()
                naughty.notify(
                    {
                        title = "Screenshot",
                        text = "Taking screenshot in 5 seconds",
                        timeout = 4,
                        icon = beautiful.screenshot_icon
                    }
                )
                awful.spawn.with_shell("sleep 5 && " .. screenshot_script)
            end
        )
    )
)
helpers.add_clickable_effect(screenshot_box)

start_screen:setup {
    -- Center boxes vertically
    nil,
    {
        -- Center boxes horizontally
        nil,
        {
            -- Column container
            {
                -- Column 1
                user_box,
                fortune_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 2
                time_box,
                notification_state_box,
                screenshot_box,
                date_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 3
                bookmarks_box,
                urls_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 4
                calendar_box,
                brightness_box,
                layout = wibox.layout.fixed.vertical
            },
            layout = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.vertical
}

local start_screen_grabber
function start_screen_hide()
    awful.keygrabber.stop(start_screen_grabber)
    start_screen.visible = false
end

local original_cursor = "left_ptr"
function start_screen_show()
    local w = _G.mouse.current_wibox
    if w then
        w.cursor = original_cursor
    end
    start_screen_grabber =
        awful.keygrabber.run(
        function(_, key, event)
            if event == "release" then
                return
            end
            if key == "Escape" or key == "q" or key == "F1" then
                start_screen_hide()
            end
        end
    )
    start_screen.visible = true
end
