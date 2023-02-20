(defun hash/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun hash/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun hash/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ))

(defun hash/configure-window-by-class ()
  (interactive)
  (message "*%s*" exwm-class-name)

  ;; Send windows to workspaces on creation
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    )
  )

(defun hash/set-wallpaper ()
  (interactive)
  (hash/run-in-background "nitrogen --restore")
  )

(defun hash/set-random-wallpaper ()
  (interactive)
  (hash/run-in-background "nitrogen --set-zoom-fill --random --save")
  )

(defun hash/exwm-init-hook ()
  ;; Make workspace 1 the default
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Random Wallpaper
  ;;(hash/set-random-wallpaper)

  ;; Start polybar
  (hash/start-panel)

  ;; Launch apps that will run in the background
  (hash/run-in-background "nm-applet")
  (hash/run-in-background "pasystray")

  ;; modeline extra
  (display-battery-mode 1)
  )

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'hash/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'hash/exwm-update-title)

  ;; Configure Windows when they are created
  (add-hook 'exwm-manage-finish-hook #'hash/configure-window-by-class)

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'hash/exwm-init-hook)

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers t)

  ;; Change minibuffer position
  ;; (setq exwm-workspace-minibuffer-position 'top)

  ;; Turn on auto focus
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  ;;(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-q") 'exwm-input-send-next-key)
  (exwm-input-set-key (kbd "s-a") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-<return>") 'eshell)
  (exwm-input-set-key (kbd "s-e") 'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-j") 'exwm-workspace-switch-to-buffer)

  ;; System tray
  ;;(require 'exwm-systemtray)
  ;;(setq exwm-systemtray-height 16)
  ;;(exwm-systemtray-enable)

  ;; Set Wallpaper
  (hash/set-wallpaper)

  (exwm-enable))

(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  )

;; Start the emacs server
  (server-start)

  (defvar hash/polybar-process nil
    "Holds the process of the running Polybar instance, if any")

  (defun hash/kill-panel ()
    (interactive)
    (when hash/polybar-process
      (ignore-errors
        (kill-process hash/polybar-process)))
    (setq hash/polybar-process nil))

  (defun hash/start-panel ()
    (interactive)
    (hash/kill-panel)
    (setq hash/polybar-process (start-process-shell-command "polybar" nil "polybar exwm-panel")))

(defun hash/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun hash/send-polybar-exwm-workspace ()
  (hash/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'hash/send-polybar-exwm-workspace)
