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
      layout              = "float";
      #split_ratio         = 0.618;
      auto_balance        = true;
      split_type          = "auto";
    };

    # Only manage Alacritty and Firefox with BSP
    extraConfig = ''
      yabai -m rule --add app!="^(Alacritty|Firefox)$" manage=off
      yabai -m config --space 2 layout bsp
      yabai -m space 2 --label bsp
    '';
  };

  services.skhd = {
    enable = true;
    skhdConfig = ''
cmd - return : open -na $(readlink "/Users/brett/Applications/Home Manager Apps/Alacritty.app")
cmd - m : open "/System/Applications/Utilities/Activity Monitor.app"
cmd + shift - return : open "/Applications/Firefox.app"

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
shift + alt - h : {yabai -m window --swap west} || {yabai -m window --toggle float; \
      yabai -m window --grid 1:2:0:0:1:1}
shift + alt - j : {yabai -m window --swap south} || {yabai -m window --toggle float; \
      yabai -m window --grid 1:2:1:0:1:1
shift + alt - k : yabai -m window --swap north
shift + alt - l : yabai -m window --swap east

# reinsert window
shift + cmd - h : yabai -m window --warp west
shift + cmd - j : yabai -m window --warp south
shift + cmd - k : yabai -m window --warp north
shift + cmd - l : yabai -m window --warp east

# make floating window fill screen

# float center
alt + shift - t : yabai -m window --toggle float; \
          yabai -m window --grid 4:4:1:1:2:2

# send window to space
shift + alt - p : yabai -m window --space prev
shift + alt - n : yabai -m window --space next
shift + alt - 1 : yabai -m window --space 1
shift + alt - 2 : yabai -m window --space 2
shift + alt - 3 : yabai -m window --space 3
shift + alt - 4 : yabai -m window --space 4
shift + alt - 5 : yabai -m window --space 5
shift + alt - 6 : yabai -m window --space 6

# focus display
ctrl + alt - p  : yabai -m display --focus prev
ctrl + alt - n  : yabai -m display --focus next
ctrl + alt - 1  : yabai -m display --focus 1
ctrl + alt - 2  : yabai -m display --focus 2
ctrl + alt - 3  : yabai -m display --focus 3

# send window to display and follow focus
ctrl + cmd - p  : yabai -m window --display prev; yabai -m display --focus prev
ctrl + cmd - n  : yabai -m window --display next; yabai -m display --focus next
ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1
ctrl + cmd - 2  : yabai -m window --display 2; yabai -m display --focus 2
ctrl + cmd - 3  : yabai -m window --display 3; yabai -m display --focus 3

# rotate tree
alt - r : yabai -m space --rotate 90

# mirror tree y-axis
alt - m : yabai -m space --mirror y-axis

# mirror tree x-axis
shift + alt - m : yabai -m space --mirror x-axis

# toggle window fullscreen
alt - f : yabai -m window --grid 1:1:0:0:1:1 || yabai -m window --toggle zoom-fullscreen

# toggle window native fullscreen
shift + alt - f : yabai -m window --toggle native-fullscreen

# toggle window parent zoom
alt - z : yabai -m window --toggle zoom-parent

# toggle window split type
alt - e : yabai -m window --toggle split

# change layout of desktop
ctrl + alt - a : yabai -m space --layout bsp
ctrl + alt - d : yabai -m space --layout float

# restart yabai
ctrl + alt - r : launchctl stop org.nixos.yabai && launchctl start org.nixos.yabai
'';
  };

  system.defaults.dock.mru-spaces = false;
}
