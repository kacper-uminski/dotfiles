from libqtile.config import Click, Drag, Group, Match, Key, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from typing import List  # noqa: F401
import os, subprocess

# Autostart Pragrams
@hook.subscribe.startup_once

def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

# Set Colors
colors = [
        '#100e23',
        '#ff8080',
        '#95ffa4',
        '#ffe9aa',
        '#91ddff',
        '#c991e1',
        '#aaffe4',
        '#cbe3e7',
        '#565575',
        '#ff5458',
        '#62d196',
        '#ffb378',
        '#65b2ff',
        '#906cff',
        '#63f2f1',
        '#a6b3cc',
        '#cbe3e7',
        '#1b182c',
        '#fbfcfc',
        ]

# Set Mod Key
mod = "mod4"

# Key definitions
keys = [

    # Switch between windows in current stack pane
    Key([mod], "h", lazy.layout.down()),
    Key([mod], "l", lazy.layout.up()),

    # Move windows up or down in current stack
    Key([mod, "control"], "h", lazy.layout.shuffle_down()),
    Key([mod, "control"], "l", lazy.layout.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key([mod], "space", lazy.layout.next()),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "w", lazy.window.kill()),

    # Exit or restart qtile.
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),

    # Start dmenu.
    Key([mod], "r", lazy.spawn("dmenu_run -l 10")),

    # Start rofi.
    #Key([mod], "r", lazy.spawn("rofi -show run -theme challenger-deep")),
    
    # Start programs.
    Key([mod], "t", lazy.spawn("alacritty")),
    Key([mod], "b", lazy.spawn("brave")),
    Key([mod], "e", lazy.spawn("emacs")),

    # Change keyboard loyouts.
    Key([mod, "mod1"], "d", lazy.spawn("setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'")),
    Key([mod, "mod1"], "s", lazy.spawn("setxkbmap -layout 'se' -variant 'dvorak' -option 'ctrl:swapcaps'")),
    Key([mod, "mod1"], "p", lazy.spawn("setxkbmap -layout 'pl' -variant 'dvorak' -option 'ctrl:swapcaps'")),

]

# Set groups, group names, and matching windows.
groups = [
        Group("1", label="DEV", layout="monadtall", matches=[Match(wm_class=["Alacritty"])]),
        Group("2", label="WEB", layout="max", matches=[Match(wm_class=["Brave-browser"])]),
        Group("3", label="MUSIC",),
        Group("4", label="CHAT",),
        Group("5", label="GAME",),
        Group("6", label="EDIT",),
        Group("7", label="DOWNLOAD",),
        Group("8", label="08",),
        Group("9", label="09",),
        Group("0", label="10",),
        ]

for i in groups:
    keys.extend([
        # mod + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen()),

        # mod + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

# Default layout theme.
layout_theme = {"border_focus": colors[13],
                "border_normal": colors[17],
                "border_width": 2,
                "margin": 35,
                }

layouts = [
    layout.Max(),
    layout.MonadTall(**layout_theme),
]

widget_defaults = dict(
    font = 'Blex Mono Nerd Font',
    fontsize = 16,
    foreground = colors[16],
    padding = 3,
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top = bar.Bar(
            [
                widget.GroupBox(
                    active = colors[16],
                    this_current_screen_border = colors[13],
                    borderwidth = 0,
                    foreground = colors[12],
                    hide_unused = True,
                    highlight_color = colors[17],
                    highlight_method = 'block',
                    margin_y = -2,
                    rounded = False,
                    urgent_border = colors[1],
                    ),

                widget.CurrentLayout(
                    background = colors[13],
                    padding = 5
                    ),

                widget.TextBox(
                    background = colors[17],
                    foreground = colors[13],
                    fontsize= 37,
                    padding= -5,
                    text= '',
                    ),

                widget.Spacer(),

                widget.TextBox(
                    background = colors[17],
                    foreground = colors[13],
                    fontsize= 37,
                    padding= -5,
                    text= '',
                    ),
 
#                widget.Net(
#                    background = colors[13],
#                    foreground = colors[16],
#                    interface = 'enp3s0',
#                    ),
 
                widget.TextBox(
                    background = colors[13],
                    foreground = colors[17],
                    fontsize= 37,
                    padding= -5,
                    text= '',
                    ),
 
                widget.TextBox(
                        background = colors[17],
                        fontsize = 24,
                        foreground = colors[16],
                        padding = 0,
                        text="墳 ",
                        ),

                widget.Volume(
                        foreground = colors[16],
                        background = colors[17],
                        padding = 5
                        ),

                widget.TextBox(
                    background = colors[17],
                    foreground = colors[13],
                    fontsize= 37,
                    padding= -5,
                    text= '',
                    ),

                widget.Clock(
                   background = colors[13],
                   format = "[%m-%d]-[ %H:%M ]",
                   ),
 
                widget.TextBox(
                    background = colors[13],
                    foreground = colors[17],
                    fontsize= 37,
                    padding= -5,
                    text= '',
                    ),

                widget.Systray(),
            ],
            25,
            background = colors[17],
            opacity = 0.90,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname':   'branchdialog'},  # gitk
    {'wname':   'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
