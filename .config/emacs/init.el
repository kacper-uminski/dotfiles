;; Move customization variables to a separate file and load it.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Suppress compilation warnings
(setq warning-minimum-level :error)

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
(set-frame-parameter (selected-frame) 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;(setq display-line-numbers-type 'relative)
;; Visual lines
(global-visual-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(dired-mode-hook
		doc-view-mode-hook
		eshell-mode-hook
		comint-mode-hook
		inferior-python-mode-hook
                org-mode-hook
		pdf-view-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Electric pair mode (completes parentheses, quotes, etc.)
(electric-pair-mode 1)


;; Treesitter
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
	(c-mode . c-ts-mode)
	(cmake-mode . cmake-ts-mode)
	(java-mode . java-ts-mode)
	(json-mode . json-ts-mode)
	(python-mode . python-ts-mode)
	(rust-mode . rust-ts-mode)
	(toml-mode . toml-ts-mode)
	(yaml-mode . yaml-ts-mode)))

;; Spell checking
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=sv_SE"))

;; Initialize Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
 

;; Initialize use-package
(require 'use-package)

;; Packages

;; Alchemist - Tooling integration for elixir
(use-package alchemist
  :defer t)

;; All The Icons - For Doom Modeline and Dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; BQN
(use-package bqn-mode
  :defer t)

;; Cider - Clojure Interactive Development Environment that Rocks
(use-package cider
  :defer t)

;; Clang-format
(use-package clang-format
  :defer t
  :custom
  (clang-format-style "file"))

;; Clang-format+ - Hooks to format buffer on save.
(use-package clang-format+
  :defer t
  :hook ((c-ts-mode
	  c++-mode) . clang-format+-mode)
  :bind
  (:map c-mode-base-map ("C-c f b" . clang-format-buffer))
  (:map c-ts-mode-map ("C-c f b" . clang-format-buffer)))

;; Clojure Mode
(use-package clojure-mode
  :defer t)

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
(use-package elixir-mode
  :defer t)

;; Erlang mode
(use-package erlang
  :defer t)

;; Evil mode
(use-package evil
  :after
  dired
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :bind
  (:map evil-insert-state-map
   ;; Return to normal mode
   ("C-g" . 'evil-normal-state)
   ;; Delete backwards
   ("C-h" . 'evil-delete-backward-char-and-join)
   ;; Output Swedish letters
   ("M-'" . (lambda () (interactive) (insert "å")))
   ("M-," . (lambda () (interactive) (insert "ä")))
   ("M-." . (lambda () (interactive) (insert "ö")))
   ("M-\"" . (lambda () (interactive) (insert "Å")))
   ("M-<" . (lambda () (interactive) (insert "Ä")))
   ("M->" . (lambda () (interactive) (insert "Ö")))
   
   :map evil-visual-state-map
   ("C-g" . 'evil-normal-state)
   )
  :custom
  (evil-respect-visual-line-mode 1)
  :config
  (evil-mode 1))

;; Evil collection (various keybindings)
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Flycheck (auto complete)
(use-package flycheck
  :init (global-flycheck-mode))

;; APL
(use-package gnu-apl-mode
  :defer t)

;; Gnu Plot - Plotting library
(use-package gnuplot
  :defer t)

;; Haskell mode
(use-package haskell-mode
  :defer t)

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
(use-package julia-mode
  :defer t)

;; Language Server Protocol
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :custom
  ;; Set prefix for lsp-command-keycap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  (lsp-ui-doc-position 'bottom)
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook
  ((c-ts-mode
    c++-mode
    clojure-mode
    elixir-mode
    erlang-mode
    python-mode
    haskell-mode
    java-mode
    julia-mode
    rust-mode
    vhdl-mode) . lsp)
  (lsp-mode . #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

;; Install for languages
(use-package lsp-haskell)

(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-julia
  :defer t)

(use-package lsp-java
  :defer t)

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
  (modus-themes-common-palette-overrides
   '(;(bg-main "#000000")
     ;(bg-line-number-inactive "#000000")
     (border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  :config
  (load-theme 'modus-vivendi-tinted))

;; LaTeX
(use-package latex
  :straight nil
  :defer t
  :mode
  ("\\.tex\\'" . latex-mode)
  :ensure auctex
  :hook
  (LaTeX-mode . turn-on-reftex)
  :custom
  (LaTeX-electric-left-right-brace t)
  (TeX-command-extra-options " -shell-escape")
  (reftex-plug-into-AUCTeX t))

;; Latex Preview Pane
;(use-package latex-preview-pane
;  :defer t)

;; Magit
(use-package magit
  :defer t)

;; Nix Mode
(use-package nix-mode
  :defer t)

;; Org
(use-package org
  :defer t
  :init (require 'org-indent)
  :custom
  (org-latex-create-formula-image-program 'dvisvgm)
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

  ;; Scale LaTeX diagrams.
  (plist-put org-format-latex-options :scale 0.5))

(use-package org-bullets
  :defer t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :init
;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))


;; Org Roam
(use-package org-roam
  :defer t
  :custom
  (org-roam-directory "/home/kacper/Documents/wiki")
  (org-roam-completions-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-pont))
  :config (org-roam-db-autosync-mode)
  :hook ('org-roam-buffer-postrender-functions
	 . (lambda () (org--latex-preview-region (point-min) (point-max)))))

;; PDF tools
(use-package pdf-tools
  :defer t)

;; Rainbow mode (visualize color codes.)
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Rainbow delimiters (parentheses highlighting)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rustic - Rust editing mode.
(use-package rustic
  :defer t)

;; Swiper
(use-package swiper)

;; Treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Typst
(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :config
  ;; Set ',."<> to output åäöÅÄÖ in insert mode
  :custom
  (typst-ts-mode-watch-options "--open"))

;; Uiua
(use-package uiua-ts-mode
  :defer t
  :mode "\\.ua\\'")

;; VHDL
(use-package vhdl-ext
  :defer t
  :custom
  (vhdl-ext-lsp-server-ids . '(ve-rust-hdl))
  :init
  (vhdl-ext-mode-setup)
  :hook
  (vhdl-mode . (lambda () (electric-pair-mode -1)) )
  (vhdl-mode . vhdl-ext-mode))

;; Which key (keychord help)
(use-package which-key
  :init (which-key-mode 1)
  :diminish which-key-mode
  :custom (which-key-idle-delay 1))

;; Yasnippet - Function templates
(use-package yasnippet
  :custom (yas-snippet-dir (concat user-emacs-directory "snippets"))
  :bind ("M-z" . yas-expand)
  :hook ((prog-mode) . yas-minor-mode-on))

(use-package yasnippet-snippets)

;;; init.el ends here
