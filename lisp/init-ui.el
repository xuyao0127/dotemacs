;;; init-ui.el --- ui configurations
;;; Commentary:
;;; Code:
(set-fontset-font t 'han (font-spec :family "Sarasa Mono SC" :size 16))
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
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; better scroll behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; ;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90))
;; (defun toggle-transparency ()
;;    (interactive)
;;    (let ((alpha (frame-parameter nil 'alpha)))
;;      (set-frame-parameter
;;       nil 'alpha
;;       (if (eql (cond ((numberp alpha) alpha)
;;                      ((numberp (cdr alpha)) (cdr alpha))
;;                      ;; Also handle undocumented (<active> <inactive>) form.
;;                      ((numberp (cadr alpha)) (cadr alpha)))
;;                100)
;;           '(90 . 90) '(100 . 100)))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)

(provide 'init-ui)
;;; init-ui.el ends here
