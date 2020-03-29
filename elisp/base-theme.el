(use-package sml-mode


 :diminish
; :general
; :config
)

;; I'm activating mini-modeline after smart-mode-line
(use-package mini-modeline
  :load-path "~/.emacs.d/plugin/emacs-mini-modeline"
  :after sml-mode
  :config
  (display-time-mode t)
  (winum-mode t)
  (setq display-time-string-forms
        '((propertize (format-time-string "%H:%M" now) 'face '((:background "#140004" :foreground "green")))))
  ;;   (setq mini-modeline-r-format
  ;; 	'("%e"
  ;; 	  mode-line-front-space
  ;; 	  ;; mode-line-mule-info
  ;; 	  mode-line-client
  ;;           my--mode-line-winum
  ;; 	  mode-line-modified
  ;; ;	  " "
  ;; ;	  display-time-string
  ;; ;	  " "
  ;; 	  mode-line-remote
  ;; 	  mode-line-frame-identification
  ;; 	  ;; mode-line-buffer-identification
  ;; 	  " "
  ;; 	  mode-line-position
  ;; 	  (vc-mode vc-mode)
  ;; 	  " "
  ;; 	  evil-mode-line-tag
  ;; 	  ;; mode-line-modes
  ;; 	  mode-line-misc-info
  ;; 	  mode-line-end-spaces))
  (defun my--pdfview-page-number ()
    (format "(%d/%d)"
	    (eval `(doc-view-current-page))
	    (doc-view-active-pages)))

  (defvar my--mode-line-line-column
    '(:eval (if (eq major-mode 'doc-view-mode)
		(my--pdfview-page-number)
	      (if (and
		   (boundp 'column-number-indicator-zero-based)
		   (not column-number-indicator-zero-based))
		  "(%l:%2c)"
		"(%l:%2c)"))))

  (defvar my--mode-line-pomodoro
    '(:eval
      (if (eq my--selected-window (selected-window))
	  `,pomodoro-mode-line-string)))

  (defvar my--mode-line-eyebrowse
    '(:eval (eyebrowse-mode-line-indicator)))

  (defvar my--mode-line-winum
    '(:eval (winum-get-number-string)))

  (defvar my--mode-line-evil-tag
    '(:eval evil-mode-line-tag))

  (setq-default mini-modeline-r-format
		(list
		 mode-line-front-space
		 "["
		 my--mode-line-winum
		 "]"
		 " %*"
		 my--mode-line-evil-tag
		 ;; line and column
		 my--mode-line-line-column
		 '(vc-mode vc-mode)
		 mode-line-process
		 ;;global-mode-string, org-timer-set-timer in org-mode need this
		 "%M"
		 " "
		 my--mode-line-pomodoro
		 ))

  (defvar my--selected-window nil)

  (defun my--record-selected-window ()
    (setq my--selected-window (selected-window)))

  (defun my--update-all ()
    (force-mode-line-update t))

  (add-hook 'post-command-hook 'my--record-selected-window)

  (add-hook 'buffer-list-update-hook 'my--update-all)
  (mini-modeline-mode t)
  )

(global-prettify-symbols-mode +1)
(setq org-ellipsis "▼")

(add-hook 'org-mode-hook
              (lambda ()
                ;; (push '("TODO"  . ?⏹) prettify-symbols-alist)
                (push '("NEXT"  . ?☞) prettify-symbols-alist)
                (push '("MEETING"  . ?📲) prettify-symbols-alist)
                (push '("DONE"  . ?✓) prettify-symbols-alist)
                (push '("CANCELLED"  . ?✘) prettify-symbols-alist)
))
(add-hook 'org-mode-hook (lambda ()
			   "Beautify Org Checkbox Symbol"
			   (push '("[ ]" . "☐") prettify-symbols-alist)
			   (push '("[X]" . "☑" ) prettify-symbols-alist)
			   (push '("[-]" . "❍" ) prettify-symbols-alist)
			   ))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;  (menu-bar-mode -1)
  (when (not (my/phone-p))
     (scroll-bar-mode -1))
  (tool-bar-mode -1)
  (winner-mode 1)
  (setq initial-frame-alist (quote ((fullscreen . maximized))))

(provide 'base-theme)
;;; base-theme ends here
