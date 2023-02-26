;;; Nathan's Emacs Settings

;; Initialize package repositories
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Refresh packages list if new install
(unless package-archive-contents
  (package-refresh-contents))

;; Require use-package; all other packages will be installed with use-package
;; See: https://jwiegley.github.io/use-package/keywords/
(setq required-package-list '(use-package))
(dolist (package required-package-list)
  (unless (package-installed-p package)
    (package-install package)))
(require 'use-package)
;; Download package if not found locally
(setq use-package-always-ensure t)

;; Plugins

;; ================================================================================================
(use-package zenburn-theme
  :init (load-theme 'zenburn t))

;; Navigate between windows
(use-package ace-window
  :init (setq aw-background nil)
  :config (global-set-key (kbd "M-o") 'ace-window))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package easy-kill
  :config (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package undo-tree
  :init  (setq undo-tree-auto-save-history nil)
  :config (global-undo-tree-mode))

;; Git interface
(use-package magit)

;; TODO: this doesn't work
;;(use-package diff-hl
;;  :config (global-diff-hl-mode))

;; Buffer navigation
;; TODO: not sure if I actually want any of this
;; (use-package avy)

;; Useful helper functions for editing
(use-package crux)

(use-package which-key
  :config (which-key-mode))

;; Auto-save
(use-package super-save
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (setq super-save-auto-save-when-idle t))

;; ================================================================================================
;; Initialize vertico, consult, marginalia, embark, orderless

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :init (setq completion-styles '(orderless substring basic)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


;; TODO: Am I going to use this?
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ================================================================================================

;; Disable the UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 0)
(toggle-scroll-bar -1)
;; Move mode-line to the top
;;(setq-default header-line-format mode-line-format)
;;(setq-default mode-line-format nil)

;; Disable backup files and auto save files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable windows beep
(setq visible-bell 1)

;; TODO: How to not make this look terribe?
;; (global-display-line-numbers-mode)

;; Show column number
(setq column-number-mode t)

;; Show matching paren when highlighted
(show-paren-mode)
(electric-pair-mode)

;; TODO: do I want this???
;; Unix line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Use spaces
(setq-default indent-tabs-mode nil)

;; TODO: use DejaVu Sans Mono
;; Set font (windows)
(set-face-attribute 'default nil :family "Consolas" :height 110)

;; Disable start screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Default open last file
(desktop-save-mode 1)

;; ================================================================================================
