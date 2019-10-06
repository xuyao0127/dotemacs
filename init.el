;;; init.el --- configuration entry point. -*- lexical-binding: t -*-

;;; Commentary:

;; set up global settings and load other files.

;;; Code:

;; Add 'lisp' folder into path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmark)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 200 1024 1024))
      (init-gc-cons-threshold (* 1280 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; load modules
(require 'init-packages)
(require 'init-ui)
(require 'init-editor)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2E3440" :foreground "#ECEFF4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 142 :width normal :foundry "outline" :family "InputMonoCondensed"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(diff-hl-change ((t (:background "#3a81c3" :foreground "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363" :foreground "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c" :foreground "#7ccd7c")))))
