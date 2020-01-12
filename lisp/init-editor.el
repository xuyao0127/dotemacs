;;; init-editor.el --- enhanced core editing experience.
;;; Commentary:
;;; Code:
;;; Code:

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
(setq-default scroll-preserve-screen-position 'always)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'after-init-hook 'global-auto-revert-mode)

;; clean up obsolete buffers automatically
(require 'midnight)

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(recentf-mode 1)
(set-language-environment "UTF-8")
(setq initial-scratch-message ";; scratch\n")
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(provide 'init-editor)
;;; init-editor.el ends here
