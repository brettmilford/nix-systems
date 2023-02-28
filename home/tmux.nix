{
  programs.tmux = {
    extraConfig = ''
if-shell "test \$TERM = \'linux\'" "set -g default-terminal \'screen.linux\'" \
    "set -g default-terminal \'tmux-256color\'"

if-shell "test \$TERM != \'linux\'" {
    # Copy to system clip
    bind-key -T copy-mode-vi WheelUpPane send-keys -X halfpage-up
    bind-key -T copy-mode-vi WheelDownPane send-keys -X halfpage-down
    # may not be necessary on headdirectver
    bind-key -T copy-mode-vi "y" send-keys -X copy-pipe-and-cancel \'xsel -i --clipboard\'
    bind-key -T copy-mode-vi "MouseDragEnd1Pane" send-keys -X copy-pipe-and-cancel \'xsel -i --clipboard\'
} {
    bind -T copy-mode-vi \'y\' send-keys -X copy-pipe-and-cancel \'reattach-to-user-namespace pbcopy\'
    bind -T copy-mode-vi \'MouseDragEnd1Pane\' send-keys -X copy-pipe-and-cancel \'reattach-to-user-namespace pbcopy\'
}

set -ga terminal-overrides "xterm*:smcup@:rmcup@,xterm*:Tc,xterm*:RGB,alacritty*:Tc,alacritty*:RGB"
set -g status-interval 1
set -g renumber-windows on
set -g visual-activity off
set -g set-titles on
set -g set-titles-string "#T - #W"
set -g display-time 1000
set -g display-panes-time 1500
set -sg escape-time 0
set -wg monitor-activity off
set -wg pane-base-index 1
set -wg automatic-rename on
set -g status on
bind-key c new-window -c "#{pane_current_path}"
bind-key v split-window -h -c "#{pane_current_path}"
bind-key s split-window -v -c "#{pane_current_path}"
bind-key C new-session
bind-key S switch-client -l
bind-key r source-file ~/.tmux.conf \; display "Config Reloaded!"

#Theme
Red=colour1 #base08
Green=colour2 #base0B
Yellow=colour3 #base0A
Blue=colour4 #base
Magenta=colour5
Cyan=colour6
Orange=colour16 #base09
Red_Orange=colour17
#shades of grey
base00=colour0
colour_bg=colour0 #Black
base01=colour10
base02=colour2
base03=colour8 #Bright Black
#shades of white
base04=colour20
base05=colour7
colour_fg=colour7 #White
base06=colour21
base07=colour15 #BrightWhite

#colour combinations
red_line="#[fg=$base00,bg=$Red]"
pl_red_green="#[fg=$Red,bg=$Green]"
green_line="#[fg=$base01,bg=$Green]"
pl_green_grey="#[fg=$Green,bg=$base01]"
pl_red_blue="#[fg=$Red,bg=$Blue]"
blue_line="#[fg=$base01,bg=$Blue]"
pl_blue_grey="#[fg=$Blue,bg=$base01]"
light_grey_line="#[fg=$colour_fg,bg=$base03]"
pl_dark_grey="#[fg=$base01,bg=$colour_bg]"

# Basic status bar colors
if-shell "test \$(uname) = \'Darwin\'" {
  dark_grey_line="#[fg=$colour_fg,bg=$base01]"
  set -g status-fg $colour_fg
  set -g status-left " #[fg=$base03]#P |"
  set -g status-right "%H:%M $blue_line %a %d-%b-%y $red_line"
  set -g window-status-separator "#[fg=$base03]|"
} {
  dark_grey_line="#[fg=$colour_fg,nobold,bg=$base01]"
  set -g status-fg $base03
  set -g status-left "#[fg=$base03,bg=$base01] #P |"
  set -g status-right "#(cat ~/.thyme-tmux) $dark_grey_line %H:%M $blue_line %d-%b-%y $red_line"
  set -g window-status-separator "|"
}
set -g status-bg $base01 #shade above terminal background
set -g status-justify left
set -g window-status-format " #[fg=$Red]#I:#W#F "
set -g window-status-current-format " [#W]#F "
set -g window-status-current-style fg=$Green,bg=$base01
set -g window-status-current-style bg=$base01,fg=$Red
set -g pane-border-style fg=$base02,bg=$base01
set -g pane-active-border-style fg=$base03,bg=$base01
set -g display-panes-colour $base02
set -g display-panes-active-colour $Green
set -g clock-mode-colour $Blue
set -g clock-mode-style 24
set -g message-style fg=$base01,bg=$Green
set -g message-command-style fg=$base01,bg=$Green
set -g mode-style fg=$base01,bg=$Blue
'';
    enable = true;
    aggressiveResize = true;
    baseIndex = 1;
    customPaneNavigationAndResize = true;
    escapeTime = 0;
    historyLimit = 20000;
    keyMode = "vi";
    #mouse = true;
    newSession = true;
  };
}
