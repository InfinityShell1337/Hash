(setq inhibit-startup-message t) ; Remove start page

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1) ; Disable the menu bar

(setq visible-bell t) ; Visible bell

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts

(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 100)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(column-number-mode)
(global-display-line-numbers-mode t)

; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-themes)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "22:00"))

(use-package all-the-icons)

(use-package term)

(defun hash/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-banner-message ""))

(use-package eshell
  :hook (eshell-first-time-mode . hash/configure-eshell))

(use-package eshell-did-you-mean)

(use-package esh-help)

(use-package eshell-z)

(use-package eshell-up)

;; (use-package eshell-git-prompt
;;   :config
;;   (eshell-git-prompt-use-theme 'robbyrussell))

(use-package eshell-prompt-extras
  :after eshell
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat
;;          (propertize (user-login-name) 'face `(:foreground "red"))
;;          (propertize "@" 'face `(:foreground "green"))
;;          (propertize (system-name) 'face `(:foreground "blue"))
;;          (propertize " " 'face `(:foreground "green"))
;;          (propertize (concat (eshell/pwd)) 'face `(:foreground "black"))
;;          (propertize " ??" 'face `(:foreground "purple"))
;;          (propertize " " 'face `(:foreground "white"))
;;          )))

;; (setq eshell-prompt-regexp "^[^#$\n!%&*()]* [^#$\n!%&*()]* ?? ")

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy)

(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode)
  )

(use-package ivy-posframe
  :config
  ;; display at `ivy-posframe-style'
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1)
  )

(use-package counsel
  :bind (:map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
	 ("M-x" . 'counsel-M-x)
	 ("C-x b" . 'counsel-switch-buffer)
	 ("C-x C-f" . 'counsel-find-file))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (counsel-mode 1)
	  (setq ivy-initial-inputs-alist nil) ; Don't start searches with ^
	  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config (general-evil-setup t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-undo-system 'undo-redo)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  )

(use-package hydra
  :defer t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    ))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~")
    (setq projectile-project-search-path '("~")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(use-package org
  :hook (org-mode . hash/org-mode-setup)
  :config
  (setq org-ellipsis " ???"
	org-hide-emphasis-markers nil))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("???" "???" "???" "???" "???" "???" "???")))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . hash/org-mode-visual-fill))

(defun hash/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))

(setq make-backup-files nil)

(defun hash/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                       (expand-file-name "~/.emacs.d/"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'hash/org-babel-tangle-config)))

(defun hash/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil
        org-confirm-babel-evaluate nil
        )

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  )

(require 'org-indent)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???"))))))

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.4)
                (org-level-3 . 1.3)
                (org-level-4 . 1.2)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

(defun hash/org-present-prepare-slide (buffer-name heading)
  ;; Hide cursor
  (org-present-hide-cursor)
  )

(use-package org-present
  :init
  (add-hook 'org-present-after-navigate-functions 'hash/org-present-prepare-slide)
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/.emacs.d/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         )
  :config
  (org-roam-setup)
  )

(use-package treemacs)

(use-package undo-tree
  :init
  (global-undo-tree-mode)

  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  )

(use-package emojify
  :init
  (add-hook 'after-init-hook #'global-emojify-mode)
  )

(use-package beacon
  :init (beacon-mode 1)
  )

(use-package minimap
  :config
  (setq minimap-window-location 'right)
  )

(use-package smartparens
  :config
  (smartparens-global-mode)
  )

(use-package buffer-flip
  :config
  (setq buffer-flip-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "<mouse-9>")   'buffer-flip-forward) 
          (define-key map (kbd "<mouse-8>") 'buffer-flip-backward)
          (define-key map (kbd "ESC")     'buffer-flip-abort)
          map))
  )

(defun hash/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . hash/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
 )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package lsp-treemacs
  :after lsp
  )

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
  )

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  )

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  )

(use-package pacmacs)

(defun hash/open-emacs-conf ()
  (interactive)
  (find-file "~/.emacs.d/Emacs.org")
  )

(defun hash/open-exwm-conf ()
  (interactive)
  (find-file "~/.emacs.d/Desktop.org")
  )

(general-define-key
 "C-M-j" 'counsel-switch-buffer
 "C-x b" 'counsel-switch-buffer
 "C-M-<tab>" 'eshell
 "<mouse-9>" 'buffer-flip
 "RET" 'evil-open-below
 )

(general-create-definer hash/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(hash/leader-keys
  "t" '(:ignore t :which-key "toggle")
  "tt" '(counsel-load-theme :which-key "theme")
  "tr" '(treemacs-select-directory :which-key "treemacs")
  "tm" '(minimap-mode :which-key "minimap")

  "tl" '(:ignore t :which-key "lsp")
  "tlt" '(lsp :which-key "toggle")
  "tls" '(lsp-treemacs-symbols :which-key "symbols")

  "r" '(:ignore t :which-key "run")
  "re" '(ielm :which-key "elisp-shell")
  "rt" '(ansi-term :which-key "term")

  "c" '(:ignore t :which-key "config")
  "ce" '(hash/open-emacs-conf :which-key "Emacs.org")
  "cd" '(hash/open-exwm-conf :which-key "Desktop.org")

  "b" '(:ignore t :which-key "buffer")
  "bi" '(ibuffer :which-key "ibuffer")
  "bb" '(counsel-switch-buffer :which-key "switch")
  "bk" '(kill-this-buffer :which-key "kill")

  "o" '(:ignore t :which-key "org")
  "op" '(org-present :which-key "present")
  "ot" '(org-babel-tangle :which-key "tangle")
  "or" '(org-redisplay-inline-images :which-key "reload-images")

  "." '(counsel-find-file :which-key "file")
  "<" '(counsel-switch-buffer :which-key "buffer")
  "/" '(counsel-M-x :which-key "M-x")
  "RET" '(eshell :which-key "eshell")
  )

;; (load-theme 'doom-Iosvkem t)
;; (load-theme 'doom-horizon t)
(load-theme 'doom-outrun-electric t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-challenger-deep t)

(set-frame-parameter (selected-frame) 'alpha '(80 . 90))
(add-to-list 'default-frame-alist '(alpha . (80 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq initial-buffer-choice "~/.emacs.d/Welcome.org")
