;;; init-packages.el --- init repo and packages
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
  (("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5))))

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

;; flycheck: check syntax on the fly
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; hl-todo: highlight todos
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

;; move-text
(use-package move-text :config (move-text-default-bindings))

;; neotree
(use-package neotree
  :defer 2
  :bind ("C-c n" . 'neotree-toggle)
  :config
  (setq neo-window-fixed-size nil)
  (setq-default neo-autorefresh t))

;; PDF-tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

;; projectile
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;; rainbow parens
(use-package rainbow-delimiters
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

(use-package smex)

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

(use-package which-key
  :config
  (which-key-mode 1))

;; youdao dictionary
(use-package youdao-dictionary
  :defer t
  :init
  (setq url-automatic-caching t)
  :bind ("C-c y" . 'youdao-dictionary-search-at-point))

;; xah fly keys
(use-package xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

;; programming modes

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'")

;; SML mode
(use-package sml-mode
  :mode
  ("\\.sml\\'" . 'sml-mode)
  ("\\.lex\\'" . 'sml-lex-mode))

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

;; latex plugins
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
           #'TeX-revert-document-buffer)

;; to use pdfview with auctex
(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
       TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package company-auctex
  :defer t)

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
  (setq bibtex-completion-library-path 
        '("C:/Users/Nasser/OneDrive/Bibliography/references-pdf"
          "C:/Users/Nasser/OneDrive/Bibliography/references-etc"))

  ;; using bibtex path reference to pdf file
  (setq bibtex-completion-pdf-field "File")

  ;;open pdf with external viwer foxit
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "C:\\Program Files (x86)\\Foxit Software\\Foxit Reader\\FoxitReader.exe" nil 0 nil fpath)))

  (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(use-package magic-latex-buffer
  :hook (laxtex-mode-hook . magic-latex-buffer))

(add-hook 'LaTeX-mode-hook ;this are the hooks I want to enable during LaTeX-mode

	  (lambda()
	    (company-auctex-init); start company latex
	    (setq TeX-auto-save t) ;enable autosave on during LaTeX-mode
	    (setq TeX-parse-self t) ; enable autoparsing
	    (setq TeX-save-query nil) ; 
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t) ;use pdf-tools for default document view
	    (setq TeX-source-correlate-method 'synctex) ; enable synctex
            
	    (setq TeX-source-correlate-mode t) ; enable text-source-correlate using synctex
	    (setq-default TeX-master nil) 
	    (global-set-key (kbd "C-c C-g") 'pdf-sync-forward-search) ;sync from text to pdf
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer) ; reload pdf buffer
	    (setq reftex-plug-into-AUCTeX t) ; enable auctex
	    (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
	    (local-set-key [C-tab] 'TeX-complete-symbol) ;tex complete symbol
	    (turn-on-auto-fill) ; autofill enable for line breaks
	    (setq-local company-backends
                        (append '((company-math-symbols-latex company-latex-commands))
                                company-backends))))

(provide 'init-packages)
;;; init-packages.el ends here

