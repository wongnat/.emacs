;;; Nathan's Emacs Settings

;; Package Repos
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Required plugins
(defvar required-packages
  '(base16-theme
    helm
    auto-complete
    autopair
    evil-nerd-commenter
    buffer-move
    go-mode
    haskell-mode
    ) "Default packages")

;; Check if all packages are installed
(require 'cl)
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If not all packages are installed, refresh package database
;; and install the missing ones.
(unless (packages-installed-p)
  ;; Check for new packages (package versions)
  (message "%s" "Refreshing package databases ...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Require necessary plugins
(require 'helm)
(require 'auto-complete)
(require 'autopair)
(require 'evil-nerd-commenter)
(require 'buffer-move)

;; Enable plugins
(helm-mode 1)
(ac-config-default)
(autopair-global-mode)

;; Set theme
(load-theme 'base16-monokai t)

;; Custom keybindings
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(evilnc-default-hotkeys)

;; Disable the UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 0)
(toggle-scroll-bar -1)
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

;; Disable backup files and auto save files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable windows beep
(setq visible-bell 1)

;; Show column number
(setq column-number-mode t)

;; Show matching paren when highlighted
(show-paren-mode t)

;; Unix line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Use spaces
(setq-default indent-tabs-mode nil)

;; Set font (windows)
(set-face-attribute 'default nil :family "Consolas" :height 110)

;; Disable start screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Run gofmt on save
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Custom Settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
