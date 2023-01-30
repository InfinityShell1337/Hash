;; default setup
(setq inhibit-startup-message t) ; Remove start page

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1) ; Disable the menu bar

(setq visible-bell t) ; Visible bell

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts

;; package init
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; doom-themes
(use-package doom-themes)

;; all-the-icons
(use-package all-the-icons)

;; Ivy for completions
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

;; Counsel for cool stuff
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

;; ivy-rich
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

;; helpful
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

;; General
(use-package general
  :config (general-evil-setup t))

(general-create-definer hash/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-M-<tab>")

;; Which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Evil
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
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil collection for better evil
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; hydra
(use-package hydra
  :defer t)

;; projectile
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

;; counsel-projectile (better ivy integration with projectile)
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; forge (extra for magit)
(use-package forge)

;; org
(use-package org)

;; Disable files~
(setq make-backup-files nil)

;; Keybindings
(hash/leader-keys
 "c" '(:ignore t :which-key "config")
 "ct" '(counsel-load-theme :which-key "theme")

 "r" '(:ignore t :which-key "run")
 "re" '(eshell :which-key "eshell")

 "b" '(:ignore t :which-key "buffer")
 "bb" '(counsel-switch-buffer :which-key "switch")
 "bk" '(kill-buffer :which-key "kill")

 "." '(counsel-find-file :which-key "file")
 "/" '(counsel-M-x :which-key "M-x")
 )

(general-define-key
 "C-M-j" 'counsel-switch-buffer
 "C-x b" 'counsel-switch-buffer)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; theme
;(load-theme 'doom-Iosvkem t)
;(load-theme 'doom-horizon t)
(load-theme 'doom-outrun-electric t)
;(load-theme 'doom-dracula t)
;(load-theme 'doom-palenight t)
;(load-theme 'doom-challenger-deep t)

;; do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" default))
 '(package-selected-packages
   '(forge evil-magi evil-magit magit counsel-projectile projectile evil-collection undo-tree evil general helpful ivy-rich which-key rainbow-delimiters doom-themes all-the-icons doom-modeline counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
