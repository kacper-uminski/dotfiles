;; Disable bars and startup message
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;;Disable line numbers for some modes
(dolist (mode '(eshell-mode-hook
		org-mode-hook
		shell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set font
(set-face-attribute 'default nil :font "Fira Code")
(set-frame-font "Fira Code" nil t)

;; Cua
(setq cua-enable-cua-keys nil)
(cua-mode 1)

;; Electric pair mode (completes parentheses, quotes, etc.)
(electric-pair-mode 1)
;; Set backup directory
(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))

; Initialize package sources 
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			  ("org" . "https://orgmode.org/elpa/")
			  ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Packages

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with "^"

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Doom themes
(use-package doom-themes
  :config
  (load-theme 'doom-challenger-deep t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line) 
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal) 
  (evil-set-initial-state 'dashboard-mode 'normal))
  ;; Prevent Cua from conflicting with org mode
  (evil-define-key 'emacs org-mode-map (kbd "<C-return>") 'org-insert-heading-respect-content)
  (evil-define-key 'insert org-mode-map (kbd "<C-return>") 'org-insert-heading-respect-content)
  ;; Set "b" as up-directory in Dired in emacs mode
  (require 'dired) ;; Needed to make the define work on startup
  (evil-define-key 'emacs dired-mode-map (kbd "b") 'dired-up-directory)
  (evil-define-key 'emacs dired-mode-map (kbd "C-b") 'dired-up-directory)

;; Evil collection (various keybindings)
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

; Haskell mode
(use-package haskell-mode)

;; Helpful (better help menus)
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-key] . helpful-key)	      
  ([remap describe-variable] . counsel-describe-variable))
  					      
(use-package impatient-mode)
					      
;; Ivy					      
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
  :config (ivy-mode 1))

;; Ivy Rich
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Org
(use-package org
  :init (require 'org-indent)
  :hook (org-mode . (lambda () (org-indent-mode)
		               (variable-pitch-mode 1)
		               (auto-fill-mode 0)
		               (visual-line-mode 1)
		               (setq evil-auto-indent nil)))
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "FiraSans" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; Rainbow delimiters (parentheses highlighting)
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Swiper
(use-package swiper)

;; Which key (keychord help)
(use-package which-key
  :init (which-key-mode 1)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

;; Self-generated stuff below

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" default))
 '(package-selected-packages
   '(org-bullets impatient-mode which-key rainbow-delimiters ivy-rich helpful haskell-mode evil-collection evil doom-themes doom-modeline counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
