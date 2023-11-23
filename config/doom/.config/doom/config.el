;;; .doom.d/config.el -*- lexical-binding: t; -*-

(defvar config-directory "~/nix-systems/config")
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 1
      scroll-margin 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; C-x * (c e k q) (C-x * ? ? for all)
(setq calc-angle-mode 'rad
      calc-algebraic-mode t
      calc-symbolic-mode t)

;; NOTE: auto-fill is generally preferred over visual-line-mode as its faster
(add-hook! 'text-mode-hook #'+word-wrap-mode)
;; TODO: this doesn't work in markdown?
;; needs to happen before markdown mode?
;(remove-hook 'markdown-mode-hook #'auto-fill-mode)
(remove-hook 'text-mode-hook #'auto-fill-mode)
(setq vc-follow-symlinks t)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs nil)

(map! :n "/" 'swiper)
(map! :n "C-." '+eshell/toggle)

(if (display-graphic-p)
    (map!
     (:after evil
      :enm "C-h"   #'evil-window-left ;; NOTE: need to remap help
      :enm "C-j"   #'evil-window-down
      :enm "C-k"   #'evil-window-up
      :enm "C-l"   #'evil-window-right))
  (progn
    (defun evil-tmux-navigate (direction)
      (let
          ((cmd (concat "windmove-" direction)))
        (condition-case nil
            (funcall (read cmd))
          (error
           (evil-tmux-command direction)))))

    (defun evil-tmux-command (direction)
      (shell-command-to-string
        (concat "tmux select-pane -"
          (evil-tmux-direction direction))))

    (defun evil-tmux-direction (direction)
      (upcase
        (substring direction 0 1)))
    (map!
     (:after evil
       :enm "C-h"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "left"))
       :enm "C-j"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "down"))
       :enm "C-k"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "up"))
       :enm "C-l"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "right"))))))

(map! "C-x ?" 'help-command) ;; NOTE: 'SPC h .' does the same

(map! "C-x ," '(lambda() (interactive)
                 (find-file (expand-file-name "doom/.config/doom/config.el" config-directory))))

(if (and IS-MAC (display-graphic-p))
         (map! "s-n" #'make-frame
               "s-w" #'delete-frame))

(defun merge-init() (interactive)
       (ediff-merge-files (expand-file-name "doom/.config/doom/init.el" config-directory) "~/.emacs.d/init.example.el"))

;; UI & Theme

;; use light theme when running in apple_terminal
;; TODO: revist, doesn't quite work
;(unless (and (not(display-graphic-p))
;             (and (string= (getenv "TERM_PROGRAM") "Apple_Terminal")
;                  (not (string= (getenv "MACOS_DARKMODE") "true"))))
;    (load-theme (intern (concat (symbol-name doom-theme) "-light")) t))

(load-theme 'doom-horizon t)
;; Slightly increase contrast
;(set-face-attribute 'default nil :foreground "#CFCFCF")
(set-face-background 'default "undefined") ;;; Sets to terminal background

(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))

(defun toggle-theme ()
  "Toggle light/dark of the current theme."
  (interactive)
  (if (string-suffix-p "-light" (symbol-name doom-theme))
      (load-theme (intern (string-remove-suffix "-light" (symbol-name doom-theme))) t)
    (if (string-suffix-p "-dark" (symbol-name doom-theme))
      (load-theme (intern (concat (string-remove-suffix "-dark" (symbol-name doom-theme)) "-light")) t)
      (load-theme (intern (concat (symbol-name doom-theme) "-light")) t)))
  (doom/reload-theme))

(map! "C-x t" 'toggle-theme)

(after! doom-modeline
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-version nil)
  (setq doom-modeline-height 15))
;; NOTE: toggle with doom/toggle-line-numbers (SPC t l)
(setq display-line-numbers-type nil)

;; Packages
(setq projectile-discover-projects-in-directory '("~/prjs"))
(map! :leader "p ]" '+ivy/project-search)
(setq magit-repository-directories '(("~/prjs" . 3) ("~/.emacs.d" . 0)))

(after! spell-fu
  (set-face-attribute 'spell-fu-incorrect-face nil :inherit 'unspecified))

(after! ivy
 (setq ivy-count-format "(%d/%d) ")
 (setq ivy-use-virtual-buffers t))

;; TODO: should hook to dired mode
;(add-hook! 'dired-mode-hook #'(lambda ()
;                               (turn-off-evil-snipe-mode)))
(after! evil-snipe
  (evil-snipe-mode -1))

(after! company
  (setq company-idle-delay nil))

(after! projectile
  (projectile-register-project-type 'nixosflake '("flake.nix")
                                  :project-file "flake.nix"
                                  :compile "darwin-rebuild switch --flake ~/nix-systems#"
                                  :test "npm test"
                                  :run "nix develop"
                                  :test-suffix ".spec"))
(setq-hook! 'nix-mode-hook
  counsel-compile-history '("darwin-rebuild switch --flake ~/nix-systems#"))

(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?} ?}))
  (setq jsonnet-use-smie t))

(after! python
  (add-hook! 'python-mode 'semantic-mode)
  (add-hook! 'python-mode 'semantic-stickyfun-mode))

(load! "+pkm")
