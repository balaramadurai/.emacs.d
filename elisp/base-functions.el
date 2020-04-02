(defun spacemacs/system-is-mac ()
  (eq system-type 'darwin))
(defun spacemacs/system-is-linux ()
  (eq system-type 'gnu/linux))
(defun spacemacs/system-is-mswindows ()
(eq system-type 'windows-nt))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun spacemacs-lite/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from magnars
(defun spacemacs-lite/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
(message "File '%s' successfully removed" filename)))))

(defun spacemacs-lite/copy-file ()
  "Write the file under new name."
  (interactive)
(call-interactively 'write-file))

;; from magnars
  ;; http://stackoverflow.com/a/10216338/4869
(defun spacemacs-lite/sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
	      (insert fname)
	      (search-backward ":")
	      (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
					      last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
	      (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

;; from magnars
(defun spacemacs-lite/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
(message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun spacemacs//open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
   ((spacemacs/system-is-linux) (let ((process-connection-type nil))
(start-process "" nil "xdg-open" file-path)))))

(defun spacemacs-lite/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (spacemacs//open-in-external-app file-path)
(message "No file associated to this buffer.")))))

(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
			   (let ((pkg (cadr (assq name where))))
			     (when pkg
			       (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(defconst user-config-org-file "~/.emacs.d/README.org")

(defun find-user-init-file ()
  "Finds the user init file"
  (interactive)
  (find-file user-init-file))

(defun find-user-config-org-file ()
  "Finds the user config file"
  (interactive)
  (find-file user-config-org-file))

(defun load-user-init-file ()
    "loads the user init file"
    (interactive)
    (load-file user-init-file))

(defun show-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun show-home-buffer ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun show-messages-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)
;    (powerline-reset)
))

; (defadvice load-theme (before disable-themes-first activate)
;  (disable-all-themes)
;  (powerline-reset)
; )

;; Following lines to cycle through themes adapted from ivan's answer on
;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes
(setq my/themes (custom-available-themes))
(setq my/themes-index 0)

(defun my/cycle-theme ()
  "Cycles through my themes."
  (interactive)
  (setq my/themes-index (% (1+ my/themes-index) (length my/themes)))
  (my/load-indexed-theme))

(defun my/load-indexed-theme ()
  (load-theme (nth my/themes-index my/themes)))

(defun load-spacemacs-dark-theme ()
  "Loads `spacemacs-dark' theme"
  (interactive)
  (load-theme 'spacemacs-dark))

(defun load-spacemacs-light-theme ()
  "Loads `spacemacs-light' theme"
  (interactive)
  (load-theme 'spacemacs-light))

(defun load-poet-theme ()
  "Loads `poet' theme"
  (interactive)
  (load-theme 'poet))

(defun load-leuven-theme ()
  "Loads `leuven' theme"
  (interactive)
  (load-theme 'leuven))

(defun load-dichromacy-theme ()
  "Loads `dichromacy' theme"
  (interactive)
  (load-theme 'dichromacy))

(defun org-babel-tangle-append ()
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle '(4))))

(defun org-babel-tangle-append-setup ()
  "Add key-binding C-c C-v C-t for `org-babel-tangle-append'."
  (org-defkey org-mode-map (kbd "C-c C-v +") 'org-babel-tangle-append))

(add-hook 'org-mode-hook #'org-babel-tangle-append-setup)

;; from https://pages.sachachua.com/.emacs.d/Sacha.html#weekly-review

(defvar my/weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")
(defun br/extract-tasks-from-agenda (string matchers prefix line-re)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward line-re nil t)
      (let ((temp-list matchers))
        (while temp-list
          (if (save-match-data
                (string-match (car (car temp-list)) (match-string 1)))
              (progn
                (add-to-list (cdr (car temp-list)) (concat prefix (match-string 3)) t)
                (setq temp-list nil)))
          (setq temp-list (cdr temp-list)))))))
(defun br/get-previous-tasks ()
  (let (string)
    (save-window-excursion
      (org-agenda nil "W")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      ;; Get any completed tasks from the current week as well
      (org-agenda-later 1)
      (org-agenda-log-mode 16)
      (setq string (concat string "\n" (buffer-string)))
      (br/extract-tasks-from-agenda string
                                         '(("routines" . ignore)
                                           ("business" . business)
                                           ("people" . relationships)
                                           ("tasks" . emacs)
                                           ("." . life))
                                         "  - [X] "
                                        my/weekly-done-line-regexp)
      )))

(defun split-and-indirect-orgtree ()
"Splits window to the right and opens an org tree section in it"
(interactive)
(split-window-right)
(org-tree-to-indirect-buffer)
(windmove-left))


(defun kill-and-unsplit-orgtree ()
"Kills the cloned buffer and deletes the window."
(interactive)
(kill-this-buffer)
(delete-window))

(general-def
"<f3>"        'split-and-indirect-orgtree
"M-<f3>"      'kill-and-unsplit-orgtree)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents  . 5)
			  ))
  )

(provide 'base-functions)
;;; base-org ends here
