;;; init.el --- configuration entry point. -*- lexical-binding: t -*-

;;; Commentary:

;; set up global settings and load other files.

;;; Code:

;; Add 'lisp' folder into path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; load modules
(require 'init-packages)
(require 'init-ui)
(require 'init-editor)

(provide 'init)
;;; init.el ends here
