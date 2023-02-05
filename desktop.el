(defun hash/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun hash/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun hash/set-wallpaper ()
  (interactive)
  (hash/run-in-background "feh --bg-scale /usr/share/backgrounds/mountain.jpg")
  )

(defun hash/exwm-init-hook ()
  ;; Make workspace 1 the default
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Launch apps that will run in the background
  (hash/run-in-background "nm-applet")
  (hash/run-in-background "pasystray")
  (hash/run-in-background "blueman-applet")

  ;; modeline extra
  (display-battery-mode 1)
  )

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'hash/exwm-update-class)

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'hash/exwm-init-hook)

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
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

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

  (exwm-input-set-key (kbd "s-a") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-<return>") 'eshell)

  ;; System tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  ;; Set Wallpaper
  (hash/set-wallpaper)

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  )
