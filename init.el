;;; init.el --- configuration entry point. -*- lexical-binding: t -*-

;;; Commentary:

;; set up global settings and load other files.

;;; Code:

;; Add 'lisp' folder into path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(setq debug-on-error t)
(defvar emax-root (concat (expand-file-name "~") "/emax"))
  (defvar emax-bin (concat emax-root "/bin"))
  (defvar emax-bin64 (concat emax-root "/bin64"))
  (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
  (defvar emax-lisp (concat emax-root "/lisp"))

  (setq exec-path (cons emax-bin exec-path))
  (setenv "PATH" (concat emax-bin ";" (getenv "PATH")))

  (setq exec-path (cons emax-bin64 exec-path))
  (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

  (setq exec-path (cons emax-mingw64 exec-path))
  (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))

  (setenv "PATH" (concat "C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;" (getenv "PATH")))

  (dolist (dir '("~/emax/" "~/emax/bin/" "~/emax/bin64/" "~/emax/mingw64/bin/" "~/emax/lisp/" "~/emax/elpa/"))
  (add-to-list 'load-path dir))

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
 '(default ((t (:inherit nil :stipple nil :background "#2E3440" :foreground "#ECEFF4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 142 :width normal :foundry "outline" :family "cascadia code"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(diff-hl-change ((t (:background "#3a81c3" :foreground "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363" :foreground "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c" :foreground "#7ccd7c")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sml-mode undo-tree youdao-dictionary use-package smooth-scroll smex smartparens rainbow-delimiters racket-mode neotree hl-todo expand-region doom-themes doom-modeline diff-hl dashboard counsel company ace-window))))
