{
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    baseIndex = 1;
    customPaneNavigationAndResize = true;
    escapeTime = 0;
    historyLimit = 20000;
    keyMode = "vi";
    newSession = true;
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
set -gw mouse on
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
bind-key r source-file ~/.config/tmux/tmux.conf \; display "Config Reloaded!"

# Smart pane switching with awareness of Vim splits.
# See: https://github.com/christoomey/vim-tmux-navigator
is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
    | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?|emacs.*$)(diff)?$'"
#is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
#    | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"
bind-key -n C-h if-shell "$is_vim" "send-keys C-h"  "select-pane -L"
bind-key -n C-j if-shell "$is_vim" "send-keys C-j"  "select-pane -D"
bind-key -n C-k if-shell "$is_vim" "send-keys C-k"  "select-pane -U"
bind-key -n C-l if-shell "$is_vim" "send-keys C-l"  "select-pane -R"
tmux_version='$(tmux -V | sed -En "s/^tmux ([0-9]+(.[0-9]+)?).*/\1/p")'
if-shell -b '[ "$(echo "$tmux_version < 3.0" | bc)" = 1 ]' \
    "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\'  'select-pane -l'"
if-shell -b '[ "$(echo "$tmux_version >= 3.0" | bc)" = 1 ]' \
    "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\\\'  'select-pane -l'"

## colours
colour_fg=colour15
colour_bg=colour235
Silver=colour7

# Basic status bar colors
set -g status-fg $colour_fg
set -g status-bg $colour_bg #shade above terminal background

# Left status bar
if-shell 'test "$(uname)" = "Darwin"' \
    'set -g status-left " #[fg=$colour_fg]#P |"' \
    'set -g status-left "#[fg=$colour_fg,bg=$colour_bg] #P |"'

# Right side of status bar
set -g status-right "%y-%m-%d %H:%M"

# Window status
set -g status-justify left
if-shell 'test "$(uname)" = "Darwin"' 'set -g window-status-separator "#[fg=$Silver]|"' 'set -g window-status-separator "|"'

set -g window-status-format " #[fg=$colour_fg]#I:#W#F "
# format when window is in focus
set -g window-status-current-format " [#W]#F "

# Current window status
set -g window-status-current-style fg=$colour_bg,bg=$colour_fg
# Window with activity status
set -g window-status-activity-style fg=$colour_bg,bg=$Silver

# Pane border
set -g pane-border-style fg=$Silver
set -g pane-active-border-style fg=$Silver

# Pane number indicator (:display-panes)
set -g display-panes-colour $Silver
set -g display-panes-active-colour $colour_fg

# Clock mode - (bind-t)
set -g clock-mode-colour $colour_fg
set -g clock-mode-style 24

# Message - effects the message line that covers the status bar on a change
set -g message-style fg=$colour_fg,bg=$colour_bg

# Command message - changes when typing a command into the message line ':'
set -g message-command-style fg=$colour_fg,bg=$colour_bg

# Mode - effects the colours in copy mode (bind esc) 
set -g mode-style fg=$colour_fg,bg=$colour_bg
'';
  };
}
