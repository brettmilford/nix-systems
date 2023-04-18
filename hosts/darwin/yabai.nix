{
  services.yabai = {
    enable = true;
    config = {
      focus_follows_mouse = false;
      mouse_follows_focus = false;
      window_placement    = "second_child";
      window_opacity      = false;
      top_padding         = 5;
      bottom_padding      = 5;
      left_padding        = 5;
      right_padding       = 5;
      window_gap          = 5;
      layout              = "bsp";
      #split_ratio         = 0.618;
      auto_balance        = true;
      split_type          = "auto";
    };

    # Only manage Alacritty and Firefox with BSP
    extraConfig = ''
      yabai -m rule --add app!="^(Alacritty|Firefox)$" manage=off
    '';
  };

  services.skhd = {
    enable = true;
    skhdConfig = ''
# cmd - return : open -na /Applications/Alacritty.app
cmd - m : open "/Applications/Utilities/Activity Monitor.app"

# open firefox
cmd + shift - return : open /Applications/Firefox.app

# close focused window
# alt - w : yabai -m window --close
# minimize focused window
alt - w : yabai -m window --minimize

# focus window
alt - h : yabai -m window --focus west
alt - j : yabai -m window --focus south
alt - k : yabai -m window --focus north
alt - l : yabai -m window --focus east

alt - p : yabai -m window --focus prev
alt - n : yabai -m window --focus next

# equalize size of windows
shift + alt - 0 : yabai -m space --balance

# swap window
shift + alt - h : yabai -m window --swap west
shift + alt - j : yabai -m window --swap south
shift + alt - k : yabai -m window --swap north
shift + alt - l : yabai -m window --swap east

# "warp" window
#shift + cmd - h : yabai -m window --warp west
#shift + cmd - j : yabai -m window --warp south
#shift + cmd - k : yabai -m window --warp north
#shift + cmd - l : yabai -m window --warp east

# make floating window fill screen
# shift + alt - up : yabai -m window --grid 1:1:0:0:1:1

# float left
shift + alt - left : yabai -m window --toggle float; \
      yabai -m window --grid 1:2:0:0:1:1

# float right
shift + alt - right : yabai -m window --toggle float; \
      yabai -m window --grid 1:2:1:0:1:1

# float center
#alt + shift - t : yabai -m window --toggle float; \
#          yabai -m window --grid 4:4:1:1:2:2

# fast focus desktop
cmd + alt - l : yabai -m space --focus recent
cmd + alt - p : yabai -m space --focus prev
cmd + alt - n : yabai -m space --focus next
cmd + alt - 1 : yabai -m space --focus 1
cmd + alt - 2 : yabai -m space --focus 2
cmd + alt - 3 : yabai -m space --focus 3
cmd + alt - 4 : yabai -m space --focus 4
cmd + alt - 5 : yabai -m space --focus 5
cmd + alt - 6 : yabai -m space --focus 6

# send window to desktop and follow focus
# shift + alt - l : yabai -m window --send-to-desktop $(yabai get _last_active_desktop); yabai -m space --focus $(yabai get _last_active_desktop)
shift + alt - p : yabai -m window --send-to-desktop prev; yabai -m space --focus prev
shift + alt - n : yabai -m window --send-to-desktop next; yabai -m space --focus next
shift + alt - 1 : yabai -m window --send-to-desktop 1; yabai -m space --focus 1
shift + alt - 2 : yabai -m window --send-to-desktop 2; yabai -m space --focus 2
shift + alt - 3 : yabai -m window --send-to-desktop 3; yabai -m space --focus 3
shift + alt - 4 : yabai -m window --send-to-desktop 4; yabai -m space --focus 4
shift + alt - 5 : yabai -m window --send-to-desktop 5; yabai -m space --focus 5
shift + alt - 6 : yabai -m window --send-to-desktop 6; yabai -m space --focus 6

# focus monitor
ctrl + alt - p  : yabai -m display -f prev
ctrl + alt - n  : yabai -m display -f next
ctrl + alt - 1  : yabai -m display -f 1
ctrl + alt - 2  : yabai -m display -f 2
ctrl + alt - 3  : yabai -m display -f 3

# send window to monitor and follow focus
ctrl + cmd - p  : yabai -m window --send-to-monitor prev; yabai -m display -f prev
ctrl + cmd - n  : yabai -m window --send-to-monitor next; yabai -m display -f next
ctrl + cmd - 1  : yabai -m window --send-to-monitor 1; yabai -m display -f 1
ctrl + cmd - 2  : yabai -m window --send-to-monitor 2; yabai -m display -f 2
ctrl + cmd - 3  : yabai -m window --send-to-monitor 3; yabai -m display -f 3

# increase + decrease region size
# NOTE: use mouse...
#shift + alt - a : yabai -m window --use-temporary-ratio 0.05 --adjust-window-edge west; yabai -m window --use-temporary-ratio -0.05 --adjust-window-edge east
#shift + alt - s : yabai -m window --use-temporary-ratio 0.05 --adjust-window-edge south; yabai -m window --use-temporary-ratio -0.05 --adjust-window-edge north
#shift + alt - w : yabai -m window --use-temporary-ratio 0.05 --adjust-window-edge north; yabai -m window --use-temporary-ratio -0.05 --adjust-window-edge south
#shift + alt - d : yabai -m window --use-temporary-ratio 0.05 --adjust-window-edge east; yabai -m window --use-temporary-ratio -0.05 --adjust-window-edge west

# set insertion point for focused container
ctrl + alt - f : yabai -m window --use-insertion-point cancel
ctrl + alt - h : yabai -m window --use-insertion-point west
ctrl + alt - j : yabai -m window --use-insertion-point south
ctrl + alt - k : yabai -m window --use-insertion-point north
ctrl + alt - l : yabai -m window --use-insertion-point east

# rotate tree
alt - r : yabai -m space --rotate 90

# mirror tree y-axis
alt - m : yabai -m space --mirror y-axis

# mirror tree x-axis
shift + alt - m : yabai -m space --mirror x-axis

# toggle desktop offset
# NOTE: no longer exists?
#alt - a : yabai -m space --toggle offset

# toggle window fullscreen
alt - f : yabai -m window --toggle zoom-fullscreen

# toggle window native fullscreen
shift + alt - f : yabai -m window --toggle native-fullscreen

# toggle window parent zoom
alt - z : yabai -m window --toggle zoom-parent

# toggle window split type
alt - e : yabai -m window --toggle split

# toggle window fade
# NOTE: no longer exists?
# alt - q : yabai -m window --toggle fade

# toggle sticky, float and resize to picture-in-picture size
# NOTE: requires SIP partially disabled
# alt - s : yabai -m window --toggle sticky;\
#          yabai -m window --grid 5:5:4:0:1:1

# float next window to be tiled
# NOTE: Doesn't work anymore? no equiv?
# shift + alt - t : yabai set window_float_next 1

# change layout of desktop
ctrl + alt - a : yabai -m space --layout bsp
# ctrl + alt - s : yabai -m space --layout monocle
ctrl + alt - s : yabai -m space --layout stack
ctrl + alt - d : yabai -m space --layout float
ctrl + alt - r : lctlrs org.nixos.yabai
'';
  };

  system.defaults.dock.mru-spaces = false;
}
