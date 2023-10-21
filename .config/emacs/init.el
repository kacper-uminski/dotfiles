;; Move customization variables to a separate file and load it.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Set backup directory
(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Disable bars and startup message
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;; Blink cursor
(blink-cursor-mode -1)

;; Prompts:
;; Replace yes-no prompts with y-n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Set font
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 120)
;;(set-frame-font "Iosevka Extended" nil t)

;; Enable transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(dired-mode-hook
		doc-view-mode-hook
		eshell-mode-hook
		comint-mode-hook
		inferior-python-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Cua
(setq cua-enable-cua-keys nil)
(cua-mode 1)

;; Electric pair mode (completes parentheses, quotes, etc.)
(electric-pair-mode 1)
(add-hook 'vhdl-mode-hook (lambda () (electric-pair-mode -1)))
 
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Packages

;; Alchemist - Tooling integration for elixir
(use-package alchemist)

;; All The Icons - For Doom Modeline and Dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; BQN - https://github.com/museoa/bqn-mode
(use-package bqn-mode)

;; Cider - Clojure Interactive Development Environment that Rocks
(use-package cider)

;; Clang-format
(use-package clang-format
  :bind
  ("C-c f b" . 'clang-format-buffer)
  ("C-c f r" . 'clang-format-region)
  :custom
  (clang-format-style "file"))

;; Clang-format+ - Hooks to format buffer on save.
(use-package clang-format+
  :hook (c-mode-common . clang-format+-mode))

;; Clojure Mode
(use-package clojure-mode)


;; Company - For autocomplete
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom (ivy-initial-inputs-alist nil)) ;; Don't start searches with "^"

;; Dashboard
(use-package dashboard
  :custom
  (dashboard-icon-type 'all-the-icons)
  (dashboard-items '((recents . 5)))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Elixir
(use-package elixir-mode)

;; Evil mode
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
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

;; Flycheck (auto complete)
(use-package flycheck
  :init (global-flycheck-mode))

;; APL
(use-package gnu-apl-mode)

;; Gnu Plot - Plotting library
(use-package gnuplot)

;; Haskell mode
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
                                              
;; Impatient mode - Web development via local http server
;;(use-package impatient-mode)
                                              
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

;; Julia Mode
(use-package julia-mode)

(use-package lsp-julia)

;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; Set prefix for lsp-command-keycap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  (lsp-ui-doc-position 'bottom)
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook ((c-mode
	  c++-mode
	  clojure-mode
	  elixir-mode
	  python-mode
	  haskell-mode
	  java-mode
	  julia-mode
	  rust-mode) . lsp)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; Install for Haskell
(use-package lsp-haskell)

;; Install for Ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Install for Java
(use-package lsp-java)

(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  ;(modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  (modus-themes-completions '((matches . (extrabold))
 			      (selection . (semibold italic textalso))))
  (modus-themes-disable-other-themes t)
  (modus-themes-headings '((1 . (rainbow bold 1.4))
 			   (2 . (rainbow semibold 1.3))
 			   (3 . (rainbow 1.2))
 			   (t . (semilight 1.1))))
  (modus-themes-italic-constructs t)
  :config
  (load-theme 'modus-vivendi))

;; LaTeX
;; AUCTeX
(use-package tex
  :ensure auctex
  :custom
  (LaTeX-electric-left-right-brace t))

;; Latex Preview Pane
(use-package latex-preview-pane)

;; Magit
(use-package magit)

;; Nix Mode
(use-package nix-mode)

;; Org
(use-package org
  :init (require 'org-indent)
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)

  ;; Set Org LaTeX margins to 2cm.
  (org-latex-packages-alist '(("margin=2cm" "geometry" nil)))

  :hook (org-mode . (lambda () (org-indent-mode)
                               (variable-pitch-mode 1)
                               (auto-fill-mode 0)
                               (visual-line-mode 1)
                               (setq evil-auto-indent nil)))
  :config
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))

  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "IBM Plex Serif" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

  ;; Scale LaTeX diagrams by 3.
  (plist-put org-format-latex-options :scale 3))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org Roam
(use-package org-roam
  :custom
  (org-roam-directory "/home/kacper/documents/wiki")
  (org-roam-completions-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-pont))
  :config (org-roam-db-autosync-mode)
  :hook ('org-roam-buffer-postrender-functions
	 . (lambda () (org--latex-preview-region (point-min) (point-max)))))

;;PDF tools 
(use-package pdf-tools)

;; Rainbow mode (visualize color codes.)
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Rainbow delimiters (parentheses highlighting)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rust
(use-package rust-mode)

;; Swiper
(use-package swiper)

;; Which key (keychord help)
(use-package which-key
  :init (which-key-mode 1)
  :diminish which-key-mode
  :custom (which-key-idle-delay 1))

;; Yasnippet - Function templates
(use-package yasnippet
  :custom (yas-snippet-dir (concat user-emacs-directory "snippets"))
  :bind ("M-z" . yas-expand)
  :hook ((java-mode) . yas-minor-mode-on))

(use-package yasnippet-snippets)

;;; init.el ends here
