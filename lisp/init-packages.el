;;; init-packages.el --- init repo and packages
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)
(require 'package)
 (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                     (not (gnutls-available-p))))
        (proto (if no-ssl "http" "https")))
   (when no-ssl
     (warn "\
 Your version of Emacs does not support SSL connections,
 which is unsafe because it allows man-in-the-middle attacks.
 There are two things you can do about this warning:
 1. Install an Emacs version that does support SSL and be safe.
 2. Remove this warning from your init file so you won't see it again."))
   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
   (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
   (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
   (when (< emacs-major-version 24)
     ;; For important compatibility libraries like cl-lib
     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; setup packages

;; Ace windows for each windows switching
(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; all the icons
(use-package all-the-icons)

;; avy for navigation
(use-package avy
  :bind ("M-s" . avy-goto-char))

;; show the cursor when moving after big movements in the window
(use-package beacon)

;; company
(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  :bind
  (("M-C-/" . company-complete)
   :map company-mode-map
   ("M-/" . company-complete)
   :map company-active-map
   ("M-/" . company-other-backend)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

;; ;; company box mode
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Swiper/ Ivy/ Counsel
(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)
         ("C-c C-r" . ivy-resume)
         ;;("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;; scale fonts
(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

;; discover-my-major
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; diff-hl
(use-package diff-hl
  :init
  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3" :foreground "#3a81c3"))))
   '(diff-hl-delete ((t (:background "#ee6363" :foreground "#ee6363"))))
   '(diff-hl-insert ((t (:background "#7ccd7c" :foreground "#7ccd7c")))))
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode))
;; modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotreetheme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; easy-kill, add more options for M-w and mark
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; expend-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flycheck: check syntax on the fly
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; git packages
(use-package git-timemachine)
(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package git-blamed)
(use-package magit
  :bind ("C-x g". magit-status))

;; google-this
(use-package google-this
  :config
  (google-this-mode 1))

;; google-translate
(use-package google-translate
  :requires google-translate-default-ui
  :bind (("C-c t" . google-translate-at-point)
         ("C-c T" . google-translate-query-translate)))

;; hl-todo: highlight todos
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

;; hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

;; imenu-anywhere
(use-package imenu-anywhere
  :bind ("C-." . ivy-imenu-anywhere))

;; move-text
(use-package move-text
  :config
  (move-text-default-bindings))

;; neotree
(use-package neotree
  :bind ([f8] . 'neotree-toggle)
  :config
  (setq neo-window-fixed-size nil)
  (setq-default neo-autorefresh t))

;; operate on number
(use-package operate-on-number)

;; org mode related packages
(use-package org
  :pin org
  :mode ("\\.org\\'" . org-mode))
(setenv "BROWSER" "firefox")
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; PDF-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode))

;; projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;; rainbow parens
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; scratch: create scratch buffer with same modes
(use-package scratch
  :bind ("C-c s" . scratch))

;; smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

;; smex
(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; SML mode
(use-package sml-mode
  :ensure t)

;; super-save
(use-package super-save
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (setq super-save-remote-files nil)
  (setq auto-save-default nil))

;; try: try packages temparorilly
(use-package try)

;; undo-tree
(use-package undo-tree
    :ensure t
    :init
    (global-undo-tree-mode)
    :config
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t))

;; show available keybindings after you start typing
(use-package which-key
  :config
  (which-key-mode 1))

;; zop-to-char
(use-package zop-to-char
  :bind ([remap zap-to-char] . zop-to-char))

;; programming modes

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'")

;; python
(use-package anaconda-mode
  :after python
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (company python)
  (push 'company-anaconda company-backends))

;; web and js
(use-package js2-mode
  :ensure ac-js2
  :mode "\\.js\\'"
  :hook
  (js-mode-hook . js2-minor-mode)
  (js2-mode-hook . ac-js2-mode))

(use-package tern
  :hook
  (js2-mode-hook . tern-mode))

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(provide 'init-packages)
;;; init-packages.el ends here

