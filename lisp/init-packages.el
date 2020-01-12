;;; init-packages.el --- init repo and packages
;;; Commentary:
;;; Code:

;; bootstrap staright.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5))))

;; projectile
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;; winum
(use-package winum
  :config (winum-mode))

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

(use-package company-prescient)
(use-package ivy-prescient)

;; neotree
(use-package neotree
  :bind ("C-c n" . neotree-toggle)
  :config
  (setq neo-autorefresh t)
  (setq neo-window-fixed-size nil))

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
(setq inhibit-compacting-font-caches t)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon nil))

;; theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;;expand selction
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flycheck: check syntax on the fly
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; git packages
(use-package magit
  :bind ("C-x g". magit-status)
  :config (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; hl-todo: highlight todos
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

;; hungry delete
(use-package hungry-delete
  :config (global-hungry-delete-mode))
;; rainbow parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

;; which key
(use-package which-key
  :config
  (which-key-mode 1))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; programming modes

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'")

(provide 'init-packages)
;;; init-packages.el ends here

