;;; writing-mode.el --- Writing mode for distraction free writing
;;
;; Author: Bala Ramadurai
;; Created: 2019-11-06
;; Version: 1.0
;; Last updated: 2019-11-06

;;; Code:

(defgroup writing nil
  "Customization for writing mode"
  :group 'wp)

(defcustom flyspell-mode-toggle t
  "To include flyspell spell checker or not."
  :type 'boolean
  :group 'writing)

(defcustom writegood-mode-toggle t
  "To include writegood-mode or not.

This mode gives you the feature of underlining weasel words"
  :type 'boolean
  :group 'writing)

(defcustom wc-mode-toggle t
  "To include wc-mode or not.

This mode gives you the feature of underlining weasel words"
  :type 'boolean
  :group 'writing)

(defcustom olivetti-mode-toggle t
  "To include writegood-mode or not.

This mode gives you the feature of underlining weasel words"
  :type 'boolean
  :group 'writing)

(defcustom prettify-mode-toggle nil
  "To include prettify-mode or not.

This mode gives fancy substitutions of any words that you set it to. The default is off."
  :type 'boolean
  :group 'writing)

(make-variable-buffer-local
 (defvar writing-mode-status nil
   "Writing Status"))

(defun writing-mode-toggle ()
  (interactive)
  ;; Writing
  (setq writing-mode-status (not writing-mode-status))

  (if writing-mode-status
      (progn
	(if writegood-mode-toggle (writegood-mode t))
	(if wc-mode-toggle (wc-mode t))
	(if olivetti-mode-toggle (olivetti-mode 1))
	(if flyspell-mode-toggle (flyspell-mode t))
	(if prettify-mode-toggle (prettify-symbols-mode t))
	(message "Writing mode enabled! Enjoy your writing")
       )
    (progn
     (writegood-mode -1)
     (wc-mode -1)
     (olivetti-mode -1)
     (flyspell-mode -1)
     (org-tracktable-write)
     (message "Writing mode disabled! Have a nice day!")
     ))

;;  (mode-line-in-header)  
  (toggle-frame-fullscreen)
  (force-mode-line-update)
  (redraw-display)
  )

;;;###autoload
(define-minor-mode writing-mode
  "Writing Mode helps writers with a distraction free mode and spell check"
  :lighter " Writing"
  (writing-mode-toggle)
  (make-local-variable 'writing-mode-status))

(provide 'writing-mode)
