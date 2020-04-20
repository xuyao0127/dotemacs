;;; package --- Sumary
;;; Commentary:
;;; Code:

;; Setup name and email for things like git
(setq user-full-name "Yao Xu"
      user-mail-address "syxuyao@outlook.com")

;; Set a larger gc
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Startup timer
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; setup package and use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Visual setup
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)
(use-package winum
  :config (winum-mode))

;; disable startup screen
(setq inhibit-startup-screen t)

;; show full path name in modeline
(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Font
(set-frame-font "Sarasa Fixed SC 14" nil t)

;; Put backup files in temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Store custom-file separately, don't freak out when it's not found.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Ease of life
(recentf-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; smartparens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package avy
  :bind
  ("M-s" . avy-goto-char)
  :config
  (setq avy-background t))

(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :bind (("C-M-g" . magit-status)))

;; Swiper/Ivy/Counsel combo
(use-package counsel
  :init
  (counsel-mode 1)
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

;; org mode
(use-package org-ref
  :init (setq org-ref-completion-library 'org-ref-ivy-cite))

;; Daemon Mode
(require 'server)
(if (not (server-running-p)) (server-start))

(provide 'init)
;;; init.el ends here
