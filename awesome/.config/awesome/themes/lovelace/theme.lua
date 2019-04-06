local theme_name = "lovelace"
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local gfs = require("gears.filesystem")
local awful = require("awful")

local themes_path = gfs.get_themes_dir()
local theme_path = os.getenv("HOME") .. "/.config/awesome/themes/" .. theme_name
local icon_path = theme_path .. "/icons/"
local layout_icon_path = theme_path .. "/layout/"
local titlebar_icon_path = theme_path .. "/titlebar/"
local weather_icon_path = theme_path .. "/weather/"
local taglist_icon_path = theme_path .. "/taglist/"

local dpi = xresources.apply_dpi
local xrdb = xresources.get_current_theme()
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

local theme = {}

-- Set the wallpaper
theme.wallpaper = theme_path .. "/wallpaper.png"

-- Set the default font
theme.font = "Hack 10" -- TODO: pick a font

-- Get colors from Xresources
theme.xbackground = xrdb.background or "#1D1F28"
theme.xforeground = xrdb.foreground or "#FDFDFD"
theme.xcolor0 = xrdb.color0 or "#282A36"
theme.xcolor1 = xrdb.color1 or "#F37F97"
theme.xcolor2 = xrdb.color2 or "#5ADECD"
theme.xcolor3 = xrdb.color3 or "#F2A272"
theme.xcolor4 = xrdb.color4 or "#8897F4"
theme.xcolor5 = xrdb.color5 or "#C574DD"
theme.xcolor6 = xrdb.color6 or "#79E6F3"
theme.xcolor7 = xrdb.color7 or "#FDFDFD"
theme.xcolor8 = xrdb.color8 or "#414458"
theme.xcolor9 = xrdb.color9 or "#FF4971"
theme.xcolor10 = xrdb.color10 or "#18E3C8"
theme.xcolor11 = xrdb.color11 or "#FF8037"
theme.xcolor12 = xrdb.color12 or "#556FFF"
theme.xcolor13 = xrdb.color13 or "#B043D1"
theme.xcolor14 = xrdb.color14 or "#3FDCEE"
theme.xcolor15 = xrdb.color15 or "#BEBEC1"

local accent_color = theme.xcolor14
local focused_color = theme.xcolor14
local unfocused_color = theme.xcolor7
local urgent_color = theme.xcolor9

theme.bg_dark = theme.xbackground
theme.bg_normal = theme.xbackground
theme.bg_focus = theme.xbackground
theme.bg_urgent = theme.xbackground
theme.bg_minimize = theme.xcolor8
theme.bg_systray = theme.xbackground

theme.fg_normal = theme.xcolor7
theme.fg_focus = focused_color
theme.fg_urgent = urgent_color
theme.fg_minimize = theme.xcolor8

-- Gaps
theme.useless_gap = dpi(3)
theme.screen_margin = dpi(3)

-- Borders
theme.border_width = dpi(0)
theme.border_color = theme.xcolor0
theme.border_normal = theme.xcolor0
theme.border_focus = theme.xcolor0
theme.border_radius = dpi(6) -- Rounded corners

-- Titlebars
theme.titlebars_enabled = false
theme.titlebar_size = dpi(35)
theme.titlebar_title_enabled = true
theme.titlebar_font = theme.font
theme.titlebar_title_align = "center"
theme.titlebar_position = "top"
theme.titlebars_imitate_borders = false
theme.titlebar_bg = theme.xcolor0
theme.titlebar_fg_focus = theme.xcolor7
theme.titlebar_fg_normal = theme.xcolor15

-- Notifications
theme.notification_position = "top_right"
theme.notification_border_width = dpi(0)
theme.notification_border_radius = theme.border_radius
theme.notification_border_color = theme.xcolor10
theme.notification_bg = theme.xbackground
theme.notification_fg = theme.xcolor7
theme.notification_crit_bg = theme.urgent_color
theme.notification_crit_fg = theme.xcolor0
theme.notification_icon_size = dpi(100)
theme.notification_margin = dpi(15)
theme.notification_opacity = 1
theme.notification_padding = theme.screen_margin * 2
theme.notification_spacing = theme.screen_margin * 2

-- Edge snap
theme.snap_bg = theme.bg_focus
if theme.border_width == 0 then
    theme.snap_border_width = dpi(8)
else
    theme.snap_border_width = dpi(theme.border_width * 2)
end
--them.snapper_gap = theme.useless_gap

-- Tag info
theme.ntags = 10
theme.tagnames = {
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10"
}

-- Widget separator
theme.separator_text = "|"
theme.separator_fg = theme.xcolor8

-- Wibar(s) (customized in bars.lua)
theme.wibar_position = "bottom"
theme.wibar_detached = false
theme.wibar_height = dpi(32)
theme.wibar_fg = theme.xcolor7
theme.wibar_bg = theme.xcolor0 .. "00"
--theme.wibar_opacity = 0.7
theme.wibar_border_color = theme.xcolor0
theme.wibar_border_width = dpi(0)
theme.wibar_border_height = dpi(0)
theme.wibar_border_radius = theme.border_radius
theme.wibar_width = dpi(565)

-- Widgets
theme.prefix_fg = theme.xcolor8

-- Tasklist
theme.tasklist_disable_icon = true
theme.tasklist_plain_task_name = true
theme.tasklist_bg_focus = theme.xcolor0 .. "00"
theme.tasklist_fg_focus = focused_color
theme.tasklist_bg_normal = theme.xcolor0 .. "00"
theme.tasklist_fg_normal = unfocused_color
theme.tasklist_bg_minimize = theme.xcolor0 .. "00"
theme.tasklist_fg_minimize = theme.fg_minimize
theme.tasklist_bg_urgent = theme.xcolor0 .. "00"
theme.tasklist_fg_urgent = urgent_color
theme.tasklist_spacing = dpi(5)
theme.tasklist_align = "center"

-- Sidebar (customized in sidebar.lua)
theme.sidebar_bg = theme.xbackground
theme.sidebar_bg_alt = theme.xcolor0
theme.sidebar_fg = theme.xcolor7
theme.sidebar_opacity = 0.8
theme.sidebar_position = "left"
theme.sidebar_width = dpi(300)
theme.sidebar_height = screen_height
theme.sidebar_y = 0
theme.sidebar_border_radius = dpi(0)
theme.sidebar_hide_on_mouse_leave = true
theme.sidebar_show_on_mouse_edge = true
theme.hide_sidebar_on_mouse_leave = true

-- Start screen
theme.start_screen_bg = theme.xcolor0 .. "CC"
theme.start_screen_fg = theme.xcolor7
theme.start_screen_font = "Noto Sans 20"
theme.start_screen_opacity = 0.9
theme.start_screen_icon_size = dpi(180)

-- Exit screen
theme.exit_screen_bg = theme.xcolor0 .. "CC"
theme.exit_screen_fg = theme.xcolor7
theme.exit_screen_font = "Noto Sans 20"
theme.exit_screen_icon_size = dpi(180)
theme.exit_screen_opacity = 0.95

-- Nerd Font icons
theme.nerd_font_minimize_icon = ""
theme.nerd_font_maximize_icon = ""
theme.nerd_font_close_icon = ""
theme.nerd_font_cloud_icon = " "
theme.nerd_font_dcloud_icon = " "
theme.nerd_font_ncloud_icon = " "
theme.nerd_font_sun_icon = " "
theme.nerd_font_stars_icon = " "
theme.nerd_font_rain_icon = " "
theme.nerd_font_snow_icon = " "
theme.nerd_font_storm_icon = " "
theme.nerd_font_mist_icon = " "
theme.nerd_font_whatever_icon = " "
theme.nerd_font_circle_icon = ""
theme.nerd_font_dot_icon = ""
theme.nerd_font_circle_dot_icon = ""
theme.nerd_font_bell_icon = ""
theme.nerd_font_search_icon = ""
theme.nerd_font_chip_icon = "﬙"
theme.nerd_font_memory_icon = ""
theme.nerd_font_temperature_icon = "﨎"
theme.nerd_font_battery_icon = ""
theme.nerd_font_battery_charging_icon = ""
theme.nerd_font_disk_icon = ""
theme.nerd_font_volume_icon = ""
theme.nerd_font_music_icon = "ﱘ"
theme.nerd_font_playerctl_prev_icon = "玲"
theme.nerd_font_playerctl_next_icon = "怜"
theme.nerd_font_playerctl_toggle_icon = "懶"
theme.nerd_font_power_icon = "襤"
theme.nerd_font_search_icon = ""
theme.nerd_font_folder_icon = ""
-- theme.nerd_font__icon = ""

-- Application icons
theme.playerctl_toggle_icon = icon_path .. "playerctl_toggle.png"
theme.playerctl_next_icon = icon_path .. "playerctl_next.png"
theme.playerctl_prev_icon = icon_path .. "playerctl_prev.png"
theme.stats_icon = icon_path .. "stats.png"
theme.search_icon = icon_path .. "search.png"
theme.volume_icon = icon_path .. "volume.png"
theme.muted_icon = icon_path .. "muted.png"
theme.mpd_icon = icon_path .. "mpd.png"
theme.firefox_icon = icon_path .. "firefox.png"
theme.youtube_icon = icon_path .. "youtube.png"
theme.discord_icon = icon_path .. "discord.png"
theme.telegram_icon = icon_path .. "telegram.png"
theme.steam_icon = icon_path .. "steam.png"
theme.lutris_icon = icon_path .. "lutris.png"
theme.files_icon = icon_path .. "files.png"
theme.manual_icon = icon_path .. "manual.png"
theme.keyboard_icon = icon_path .. "keyboard.png"
theme.appearance_icon = icon_path .. "appearance.png"
theme.editor_icon = icon_path .. "editor.png"
theme.redshift_icon = icon_path .. "redshift.png"
theme.gimp_icon = icon_path .. "gimp.png"
theme.terminal_icon = icon_path .. "terminal.png"
theme.terminal_icon = icon_path .. "terminal.png"
theme.mail_icon = icon_path .. "mail.png"
theme.temperature_icon = icon_path .. "temperature.png"
theme.battery_icon = icon_path .. "battery.png"
theme.battery_charging_icon = icon_path .. "battery_charging.png"
theme.cpu_icon = icon_path .. "cpu.png"
theme.compositor_icon = icon_path .. "compositor.png"
theme.start_icon = icon_path .. "start.png"
theme.ram_icon = icon_path .. "ram.png"
theme.screenshot_icon = icon_path .. "screenshot.png"
theme.home_icon = icon_path .. "home.png"
theme.alarm_icon = icon_path .. "alarm.png"

-- Weather icons
theme.cloud_weather_icon = weather_icon_path .. "cloud.png"
theme.dcloud_weather_icon = weather_icon_path .. "dcloud.png"
theme.ncloud_weather_icon = weather_icon_path .. "ncloud.png"
theme.sun_weather_icon = weather_icon_path .. "sun.png"
theme.stars_weather_icon = weather_icon_path .. "stars.png"
theme.rain_weather_icon = weather_icon_path .. "rain.png"
theme.snow_weather_icon = weather_icon_path .. "snow.png"
theme.storm_weather_icon = weather_icon_path .. "storm.png"
theme.mist_weather_icon = weather_icon_path .. "mist.png"
theme.whatever_weather_icon = weather_icon_path .. "whatever.png"

-- Weather text
theme.weather_text_font = "DroidSansMono Nerd Font 14"
theme.cloud_weather_text = theme.nerd_font_cloud_icon
theme.dcloud_weather_text = theme.nerd_font_dcloud_icon
theme.ncloud_weather_text = theme.nerd_font_ncloud_icon
theme.sun_weather_text = theme.nerd_font_sun_icon
theme.stars_weather_text = theme.nerd_font_stars_icon
theme.rain_weather_text = theme.nerd_font_rain_icon
theme.snow_weather_text = theme.nerd_font_snow_icon
theme.storm_weather_text = theme.nerd_font_storm_icon
theme.mist_weather_text = theme.nerd_font_mist_icon
theme.whatever_weather_text = theme.nerd_font_whatever_icon

-- Exit screen icons
theme.exit_icon = icon_path .. "exit.png"
theme.poweroff_icon = icon_path .. "poweroff.png"
theme.reboot_icon = icon_path .. "reboot.png"
theme.suspend_icon = icon_path .. "suspend.png"
theme.lock_icon = icon_path .. "lock.png"

-- Icon taglist
theme.taglist_icons_empty = {}
theme.taglist_icons_occupied = {}
theme.taglist_icons_focused = {}
theme.taglist_icons_urgent = {}
for i = 1, theme.ntags do
    theme.taglist_icons_empty[i] = taglist_icon_path .. tostring(i) .. "_empty.png"
    theme.taglist_icons_occupied[i] = taglist_icon_path .. tostring(i) .. "_occupied.png"
    theme.taglist_icons_focused[i] = taglist_icon_path .. tostring(i) .. "_focused.png"
    theme.taglist_icons_urgent[i] = taglist_icon_path .. tostring(i) .. "_urgent.png"
end

-- Prompt
theme.prompt_fg = accent_color

-- Text taglist
theme.taglist_text_font = "DroidSansMono Nerd Font 12"
theme.taglist_text_empty = theme.nerd_font_circle_icon
theme.taglist_text_occupied = theme.nerd_font_dot_icon
theme.taglist_text_focused = theme.nerd_font_circle_dot_icon
theme.taglist_text_urgent = theme.nerd_font_bell_icon
theme.taglist_text_color_focused = theme.xcolor10
theme.taglist_text_color_occupied = theme.xcolor10
theme.taglist_text_color_empty = theme.xcolor10
theme.taglist_text_color_urgent = urgent_color
theme.taglist_disable_icon = true
theme.taglist_spacing = dpi(0)
theme.taglist_item_roundness = theme.border_radius

-- Variables set for theming the menu
theme.menu_submenu_icon = icon_path .. "submenu.png"
theme.menu_height = dpi(35)
theme.menu_width = dpi(180)
theme.menu_bg_normal = theme.xcolor0
theme.menu_fg_normal = theme.xcolor7
theme.menu_bg_focus = theme.xcolor8 .. "55"
theme.menu_fg_focus = theme.xcolor7
theme.menu_border_width = dpi(0)
theme.menu_border_color = theme.xcolor0

-- Titlebar buttons
theme.titlebar_close_button_normal = titlebar_icon_path .. "close_normal.svg"
theme.titlebar_close_button_focus = titlebar_icon_path .. "close_focus.svg"
theme.titlebar_minimize_button_normal = titlebar_icon_path .. "minimize_normal.svg"
theme.titlebar_minimize_button_focus = titlebar_icon_path .. "minimize_focus.svg"
theme.titlebar_ontop_button_normal_inactive = titlebar_icon_path .. "ontop_normal_inactive.svg"
theme.titlebar_ontop_button_focus_inactive = titlebar_icon_path .. "ontop_normal_inactive.svg"
theme.titlebar_ontop_button_normal_active = titlebar_icon_path .. "ontop_normal_active.svg"
theme.titlebar_ontop_button_focus_active = titlebar_icon_path .. "ontop_normal_active.svg"
theme.titlebar_sticky_button_normal_inactive = titlebar_icon_path .. "sticky_normal_inactive.svg"
theme.titlebar_sticky_button_focus_inactive = titlebar_icon_path .. "sticky_normal_inactive.svg"
theme.titlebar_sticky_button_normal_active = titlebar_icon_path .. "sticky_normal_active.svg"
theme.titlebar_sticky_button_focus_active = titlebar_icon_path .. "sticky_normal_active.svg"
theme.titlebar_floating_button_normal_inactive = titlebar_icon_path .. "floating_normal_inactive.svg"
theme.titlebar_floating_button_focus_inactive = titlebar_icon_path .. "floating_normal_inactive.svg"
theme.titlebar_floating_button_normal_active = titlebar_icon_path .. "floating_normal_active.svg"
theme.titlebar_floating_button_focus_active = titlebar_icon_path .. "floating_normal_active.svg"
theme.titlebar_maximized_button_normal_inactive = titlebar_icon_path .. "maximized_normal_inactive.svg"
theme.titlebar_maximized_button_focus_inactive = titlebar_icon_path .. "maximized_normal_inactive.svg"
theme.titlebar_maximized_button_normal_active = titlebar_icon_path .. "maximized_normal_active.svg"
theme.titlebar_maximized_button_focus_active = titlebar_icon_path .. "maximized_normal_active.svg"
-- (hover)
theme.titlebar_close_button_normal_hover = titlebar_icon_path .. "close_normal_hover.svg"
theme.titlebar_close_button_focus_hover = titlebar_icon_path .. "close_focus_hover.svg"
theme.titlebar_minimize_button_normal_hover = titlebar_icon_path .. "minimize_normal_hover.svg"
theme.titlebar_minimize_button_focus_hover = titlebar_icon_path .. "minimize_focus_hover.svg"
theme.titlebar_ontop_button_normal_inactive_hover = titlebar_icon_path .. "ontop_normal_inactive_hover.svg"
theme.titlebar_ontop_button_focus_inactive_hover = titlebar_icon_path .. "ontop_normal_inactive_hover.svg"
theme.titlebar_ontop_button_normal_active_hover = titlebar_icon_path .. "ontop_normal_active_hover.svg"
theme.titlebar_ontop_button_focus_active_hover = titlebar_icon_path .. "ontop_normal_active_hover.svg"
theme.titlebar_sticky_button_normal_inactive_hover = titlebar_icon_path .. "sticky_normal_inactive_hover.svg"
theme.titlebar_sticky_button_focus_inactive_hover = titlebar_icon_path .. "sticky_normal_inactive_hover.svg"
theme.titlebar_sticky_button_normal_active_hover = titlebar_icon_path .. "sticky_normal_active_hover.svg"
theme.titlebar_sticky_button_focus_active_hover = titlebar_icon_path .. "sticky_normal_active_hover.svg"
theme.titlebar_floating_button_normal_inactive_hover = titlebar_icon_path .. "floating_normal_inactive_hover.svg"
theme.titlebar_floating_button_focus_inactive_hover = titlebar_icon_path .. "floating_normal_inactive_hover.svg"
theme.titlebar_floating_button_normal_active_hover = titlebar_icon_path .. "floating_normal_active_hover.svg"
theme.titlebar_floating_button_focus_active_hover = titlebar_icon_path .. "floating_normal_active_hover.svg"
theme.titlebar_maximized_button_normal_inactive_hover = titlebar_icon_path .. "maximized_normal_inactive_hover.svg"
theme.titlebar_maximized_button_focus_inactive_hover = titlebar_icon_path .. "maximized_normal_inactive_hover.svg"
theme.titlebar_maximized_button_normal_active_hover = titlebar_icon_path .. "maximized_normal_active_hover.svg"
theme.titlebar_maximized_button_focus_active_hover = titlebar_icon_path .. "maximized_normal_active_hover.svg"

-- Layout icons
theme.layout_fairh = layout_icon_path .. "fairh.png"
theme.layout_fairv = layout_icon_path .. "fairv.png"
theme.layout_floating = layout_icon_path .. "floating.png"
theme.layout_magnifier = layout_icon_path .. "magnifier.png"
theme.layout_max = layout_icon_path .. "max.png"
theme.layout_fullscreen = layout_icon_path .. "fullscreen.png"
theme.layout_tilebottom = layout_icon_path .. "tilebottom.png"
theme.layout_tileleft = layout_icon_path .. "tileleft.png"
theme.layout_tile = layout_icon_path .. "tile.png"
theme.layout_tiletop = layout_icon_path .. "tiletop.png"
theme.layout_spiral = layout_icon_path .. "spiral.png"
theme.layout_dwindle = layout_icon_path .. "dwindle.png"
theme.layout_cornernw = layout_icon_path .. "cornernw.png"
theme.layout_cornerne = layout_icon_path .. "cornerne.png"
theme.layout_cornersw = layout_icon_path .. "cornersw.png"
theme.layout_cornerse = layout_icon_path .. "cornerse.png"

-- Minimal tasklist widget variables
theme.minimal_tasklist_visible_clients_color = focused_color
theme.minimal_tasklist_visible_clients_text = "+ "
theme.minimal_tasklist_hidden_clents_color = theme.xcolor8
theme.minimal_tasklist_hidden_clients_text = " - "

-- MPD song
theme.mpd_song_title_color = theme.xcolor7
theme.mpd_song_artist_color = theme.xcolor7
theme.mpd_song_paused_color = theme.xcolor7

-- Volume bar
theme.volume_bar_active_color = theme.xcolor6
theme.volume_bar_active_background_color = theme.xcolor6 .. "33"
theme.volume_bar_muted_color = theme.xcolor8
theme.volume_bar_muted_background_color = theme.xcolor8 .. "33"

-- Temperature bar
theme.temperature_bar_active_color = theme.xcolor1
theme.temperature_bar_background_color = theme.xcolor1 .. "33"

-- Battery bar
theme.battery_bar_active_color = theme.xcolor5
theme.battery_bar_background_color = theme.xcolor5 .. "33"

-- CPU bar
theme.cpu_bar_active_color = theme.xcolor2
theme.cpu_bar_background_color = theme.xcolor2 .. "33"

-- RAM bar
theme.ram_bar_active_color = theme.xcolor12
theme.ram_bar_background_color = theme.xcolor12 .. "33"

-- Brightness bar
theme.brightness_bar_active_color = theme.xcolor11
theme.brightness_bar_background_color = theme.xcolor11 .. "33"

-- Generate Awesome icon
theme.awesome_icon = theme_assets.awesome_icon(theme.menu_height, theme.bg_focus, theme.fg_focus)

-- Define the icon theme for application icons
local icons = "Numix"
theme.icon_theme = os.getenv("HOME") .. ".icons/" .. icons

return theme
