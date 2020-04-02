(use-package poet-theme)

(use-package spacemacs-common
    :ensure spacemacs-theme
    )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
; :general
 :config
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-continuous-word-count-modes '(org-mode))
(setq doom-modeline-modal-icon t)
(setq doom-modeline-bar-width 3)
)

(global-prettify-symbols-mode +1)
(setq org-ellipsis "▼")

(add-hook 'org-mode-hook
              (lambda ()
                ;; (push '("TODO"  . ?⏹) prettify-symbols-alist)
                (push '("NEXT"  . ?☞) prettify-symbols-alist)
                (push '("MEETING"  . ?會) prettify-symbols-alist)
                ;; (push '("DONE"  . ?✓) prettify-symbols-alist)
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
