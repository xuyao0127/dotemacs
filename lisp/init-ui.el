;;; init-ui.el --- ui configurations
;;; Commentary:
;;; Code:

(set-frame-font "Sarasa Mono SC 16")
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
(setq initial-scratch-message ";; scratch buffer\n")

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)
(global-display-line-numbers-mode)

(provide 'init-ui)
;;; init-ui.el ends here
