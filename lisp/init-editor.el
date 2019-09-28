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

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; (require 'flyspell)
;; (setq ispell-program-name "aspell" ; use aspell instead of ispell
      ;; ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;; (add-hook 'text-mode-hook 'flyspell-mode +1)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; clean up obsolete buffers automatically
;; (require 'midnight)

(provide 'init-editor)
;;; init-editor.el ends here
