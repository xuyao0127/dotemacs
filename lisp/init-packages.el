;;; init-packages.el --- init repo and packages
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; Swiper/ Ivy/ Counsel
(use-package counsel
  :init
  (counsel-mode 1)
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
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 1)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-items '((recents  . 5)
;;                         (bookmarks . 5)
;;                         (projects . 5))))

;; diff-hl
(use-package diff-hl
  :init
  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3" :foreground "#3a81c3"))))
   '(diff-hl-delete ((t (:background "#ee6363" :foreground "#ee6363"))))
   '(diff-hl-insert ((t (:background "#7ccd7c" :foreground "#7ccd7c")))))
  :hook
  (after-init . global-diff-hl-mode)
  ;; (after-init . diff-hl-flydiff-mode)
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
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotreetheme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;expand selction
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; ;; flycheck: check syntax on the fly
;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode))


;; hl-todo: highlight todos
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

;; neotree
(use-package neotree
  :defer 2
  :bind ("C-c n" . 'neotree-toggle)
  :config
  (setq neo-window-fixed-size nil)
  (setq-default neo-autorefresh t))

;; ;; projectile
;; (use-package projectile
;;   :bind ("C-c p" . projectile-command-map)
;;   :config
;;   (projectile-mode)
;;   (setq projectile-completion-system 'ivy))

;; rainbow parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

(use-package smex
  :bind ("M-x" . smex))

;; youdao dictionary
(use-package youdao-dictionary
  :defer t
  :init
  (setq url-automatic-caching t)
  :bind ("C-c y" . 'youdao-dictionary-search-at-point))


(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; programming modes

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'")

;; SML mode
(use-package sml-mode
  :mode
  ("\\.sml\\'" . 'sml-mode)
  ("\\.lex\\'" . 'sml-lex-mode))

;; ;; lsp
;; (use-package lsp-mode
;;   :hook
;;   (python-mode . 'lsp-deferred)
;;   (js-mode . 'lsp-deferred)
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-packages)
;;; init-packages.el ends here

