(use-package org
   :ensure org-plus-contrib
   :pin org
   :init
   (defun my-org-mode-hooks ()
     (visual-line-mode)
     (diminish 'visual-line-mode " Ⓥ")
;     (flyspell-mode)
;     (diminish 'flyspell-mode " Ⓕ")
;     (smartparens-mode)
     )
   (add-hook 'org-mode-hook 'my-org-mode-hooks)

   :general
   (spacemacs-lite/set-leader-keys
     "ao"   '(:ignore t :which-key "org")
     "aoc"   'org-capture
     "aol"   'org-store-link
     "aoo"   'org-agenda
     "r"     '(:ignore t :wk "Org Reviews")
     "rd"    'daily-review
     "rw"    'weekly-review
     "rq"    'quarterly-review
     "ry"    'yearly-review
     )
   (spacemacs-lite/set-leader-keys-for-major-mode
     ","    'org-time-stamp
     "!"    'org-time-stamp-inactive
     "."    'org-ctrl-c-ctrl-c
     "'"    'org-edit-special
     ":"    'org-set-tags-command
     "*"    'org-ctrl-c-star
     "a"    'org-agenda
     "A"    'org-attach
     "c"    'org-capture
     "C"    '(:ignore t :which-key "Clocks")
     "Ci"   'org-clock-in
     "Co"   'org-clock-out
     "Cq"   'org-clock-cancel
     "Cl"    'org-clock-in-last
     "e"     '(:ignore t :wk "export")
     "ee"   'org-export-dispatch
     "l"    'org-store-link
     "i"    '(:ignore t :which-key "insert")
     "id"   '(:ignore t :which-key "dates")
     "idi"     'org-time-stamp-inactive
     "ida"     'org-time-stamp
     "il"      'org-insert-link
     "d"       '(:ignore t :wk "dates")
     "ds"      'org-schedule
     "dd"      'org-deadline
     "o"       'org-open-at-point
     "r"       'org-refile
     "P"       'org-set-property
     "R"       '(:ignore t :which-key "Reviews")
     "Rd"      'daily-review
     "Rw"      'weekly-review
     "Rq"      'quarterly-review
     "Ry"      'yearly-review
     "s"       '(:ignore t :which-key "subtrees")
     "sc"      'org-copy-subtree
     "sa"      'org-archive-subtree
     "t"       'org-babel-tangle
     "<right>" 'org-agenda-do-date-later
     "<left>"  'org-agenda-do-date-earlier
     "x"       'my/org-agenda-done
     )

   (general-def org-mode-map
     "<f8>"      'org-narrow-to-subtree
     "M-<f8>"    'widen
     "<f6>"          'my/org-done
     )

   (general-define-key
    :definer 'minor-mode
    :states 'normal
    :keymaps 'org-capture-mode
    ".c"           'org-capture-finalize
    ".k"           'org-capture-kill
    ".r"           'org-capture-refile)

  (general-define-key
    :definer 'minor-mode
    :states 'normal
    :keymaps 'org-src-mode
    ".c"           'org-edit-src-exit
    ".k"           'org-edit-src-abort)

  (general-define-key
    :keymaps 'org-agenda-mode-map
    ","                              'org-agenda-goto-today
    "n"                              'org-agenda-later
    "p"                              'org-agenda-earlier
    [remap org-clock-in]             'org-agenda-clock-in
    [remap org-clock-out]            'org-agenda-clock-out
    [remap org-clock-cancel]         'org-agenda-clock-cancel
    [remap org-schedule]             'org-agenda-schedule
    [remap org-deadline]             'org-agenda-deadline)

   :config

   (defun my/org-done (&optional arg)
     "Mark current TODO as done.
   This changes the line at point, all other lines in the agenda referring to
   the same tree node, and the headline of the tree node in the Org-mode file."
     (interactive "P")
     (org-todo "DONE"))

   (defun my/org-agenda-done (&optional arg)
     "Mark current TODO as done.
   This changes the line at point, all other lines in the agenda referring to
   the same tree node, and the headline of the tree node in the Org-mode file."
     (interactive "P")
     (org-agenda-todo "DONE"))
   (defun daily-review()
     (interactive)
     (org-capture nil "rd")
     (org-capture-finallize t)
     (org-speed-move-safe 'outline-up-heading)
     (org-narrow-to-subtree)
     (fetch-calendar)
     )
   (defun weekly-review ()
     (interactive)
     (progn
       (org-capture nil "rw")
       (org-capture-finalize t)
       (org-speed-move-safe 'outline-up-heading)
       (org-narrow-to-subtree)
       (fetch-calendar)))

   (defun quarterly-review ()
     (interactive)
     (progn
       (org-capture nil "rq")
       (org-capture-finalize t)
       (org-speed-move-safe 'outline-up-heading)
       (org-narrow-to-subtree)
       (fetch-calendar)))

   (defun yearly-review ()
     (interactive)
     (progn
       (org-capture nil "ry")
       (org-capture-finalize t)
       (org-speed-move-safe 'outline-up-heading)
       (org-narrow-to-subtree)
       (fetch-calendar)))

;; (setq org-protocol-default-template-key "l")
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/inbox.org")
               "* TODO %?\nSCHEDULED: %^{Time of task?}T\n%U\n%a\n")
              ("m" "Meeting" entry (file "~/org/inbox.org")
	       (file "~/org/templates/meeting.org") :clock-in t)
              ("i" "Invoice" entry (file "~/org/inbox.org")
	       (file "~/org/templates/invoice-template.org"))
	      ("f" "New FD" entry (file+headline "~/org/Fava/ALK-finance-open-close.org" "FD")
	       (file "~/org/templates/ALK-FD.org"))
              ("p" "Project" entry (file "~/org/inbox.org")
	       (file "~/org/templates/project-template.org"))
              ("s" "Someday / Maybe Idea" entry (file+headline "~/org/somedaymaybe.org" "Someday / Maybe")
               "* SOMEDAY %?\n")
              )))

(setq org-agenda-default-appointment-duration 30)
(setq org-icalendar-store-UID t)
(setq org-icalendar-use-scheduled (quote (event-if-todo todo-start)))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)
                                 ("~/org/somedaymaybe.org" :maxlevel . 2)
)))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-agenda-hide-tags-regexp nil)
(setq org-agenda-ignore-properties (quote (effort appt stats)))
(setq org-agenda-include-diary t)
(setq org-agenda-remove-tags nil)
(setq org-agenda-span 1)
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-start-with-log-mode (quote (closed clock state)))
(setq org-agenda-use-tag-inheritance (quote (nil)))
(require 'notifications)
 ; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)
(display-time)                   ;; activate time display

(defcustom appt-notification-bus :session
   "D-Bus bus to use for notification."
   :version "25.1"
   :group 'appt-notification
   :type '(choice (const :tag "Session bus" :session) string))

(defun psachin/appt-display (min-to-app new-time msg)
   "Send notification."
   (notifications-notify :bus appt-notification-bus
                         :title (format "Appointment in %s minutes." min-to-app)
                         :body (format "%s" msg)
                         :replaces-id nil
                         :app-icon nil
                         :timeout 5000
                         :desktop-entry "emacs"))

(setq appt-disp-window-function (function psachin/appt-display))

(setq-default diary-file "~/.emacs.d/diary"
		   appt-display-format 'window
		   appt-display-duration 60
		   appt-audible t
		   appt-display-interval 3
		   appt-message-warning-time 10
		   display-time-24hr-format t
		   display-time-day-and-date t)

(run-at-time "24:01" 3600 'bh/org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt) ;; update appt list on agenda view
;; (setq diary-file "~/org/mycal.org")

(setq org-agenda-custom-commands
      (quote
       (("rw" "Weekly Review"
         ((agenda ""
                  ((org-agenda-overriding-header "This Week & The Next")
                   (org-agenda-show-all-dates t)
                   (org-agenda-archives-mode t)
                   (org-agenda-span
                    (quote fortnight))
                   ))
          (tags-todo "-CANCELLED-HOLD+Proj/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       (quote bh/skip-non-stuck-projects))
                      (org-agenda-sorting-strategy
                       (quote
                        (category-keep))))
                      )
          (tags-todo "-HOLD-CANCELLED+Proj/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function
                       (quote bh/skip-non-projects))
                      (org-tags-match-list-sublevels
                       (quote indented))
                      (org-agenda-sorting-strategy
                       (quote
                        (category-keep)))))
          (tags "SOMEDAY"
                (
                 (org-agenda-files '("~/org/somedaymaybe.org"))
                 (org-agenda-overriding-header "Someday to Inbox")))
          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header
                       (concat "Waiting and Postponed Tasks"
                               (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function
                       (quote bh/skip-non-tasks))
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         )
        ("rq" "Quarterly Review"
         ((tags-todo "-HOLD-CANCELLED+Proj/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function
                       (quote bh/skip-non-projects))
                      (org-tags-match-list-sublevels
                       (quote indented))
                      (org-agenda-sorting-strategy
                       (quote
                        (category-keep)))))
          (tags "SOMEDAY"
                (
                 (org-agenda-files '("~/org/somedaymaybe.org"))
                 (org-agenda-overriding-header "Someday to Projects")))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function
                  (quote bh/skip-non-archivable-tasks))
                 (org-tags-match-list-sublevels nil)))
          )
         )
        ("ry" "Yearly Review"
         ((tags "CYear"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-HOLD-CANCELLED+Proj/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function
                       (quote bh/skip-non-projects))
                      (org-tags-match-list-sublevels
                       (quote indented))
                      (org-agenda-sorting-strategy
                       (quote
                        (category-keep)))))
          (tags-todo "-CANCELLED+Proj/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       (quote bh/skip-non-stuck-projects))
                      (org-agenda-sorting-strategy
                       (quote
                        (category-keep)))))
          (tags "SOMEDAY"
                ((org-agenda-overriding-header "Someday to Projects"))))
         )
        ("r" . "Reviews")

  (" " agenda "Whole Agenda"
   ((org-agenda-include-diary t)
    ))
  )))

(add-hook 'org-mode-hook #'org-indent-mode)
;(add-hook 'text-mode-hook 'variable-pitch-mode)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)


;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

(setq org-enforce-todo-dependencies t)

(setq org-id-method (quote uuidgen))

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(setq org-log-note-headings '((done        . "CLOSING NOTE %t")
                              (state       . "State %-12s from %-12S %t")
                              (note        . "Note taken on %t")
                              (reschedule  . "Schedule changed on %t: %S -> %s")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline  . "Deadline changed on %t: %S -> %s")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile      . "Refiled on %t")
                              (clock-out   . "")))

(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-table-use-standard-references (quote from))

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(set-face-attribute 'org-tag nil :inherit '(fixed-pitch shadow))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(setq org-alphabetical-lists t)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'smart)

(setq org-cycle-separator-lines 0)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim)
				 ("+" '(:strike-through t :foreground "gray"))
				 ("+" org-emphasis-alist :key 'car :test 'equal))))

(setq org-use-sub-superscripts nil)

(setq org-odd-levels-only nil)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(defun org-set-line-checkbox (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (insert "- [ ] ")
      (forward-line))
    (beginning-of-line)))

;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(defun clocktable-by-tag/shift-cell (n)
  (let ((str ""))
    (dotimes (i n)
      (setq str (concat str "| ")))
    str))

(defun clocktable-by-tag/insert-tag (params)
  (let ((tag (plist-get params :tags)))
    (insert "|--\n")
    (insert (format "| %s | *Tag time* |\n" tag))
    (let ((total 0))
  (mapcar
       (lambda (file)
     (let ((clock-data (with-current-buffer (find-file-noselect file)
                 (org-clock-get-table-data (buffer-name) params))))
       (when (> (nth 1 clock-data) 0)
         (setq total (+ total (nth 1 clock-data)))
         (insert (format "| | File *%s* | %.2f |\n"
                 (file-name-nondirectory file)
                 (/ (nth 1 clock-data) 60.0)))
         (dolist (entry (nth 2 clock-data))
           (insert (format "| | . %s%s | %s %.2f |\n"
                   (org-clocktable-indent-string (nth 0 entry))
                   (nth 1 entry)
                   (clocktable-by-tag/shift-cell (nth 0 entry))
                   (/ (nth 3 entry) 60.0)))))))
       (org-agenda-files))
      (save-excursion
    (re-search-backward "*Tag time*")
    (org-table-next-field)
    (org-table-blank-field)
    (insert (format "*%.2f*" (/ total 60.0)))))
    (org-table-align)))

(defun org-dblock-write:clocktable-by-tag (params)
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|     |          | <r>  |\n")
  (let ((tags (plist-get params :tags)))
    (mapcar (lambda (tag)
          (setq params (plist-put params :tags tag))
          (clocktable-by-tag/insert-tag params))
        tags)))

(defun my-tbl-export (name)
  "Search for table named `NAME` and export."
  (interactive "s")
  (show-all)
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
    (progn
      (next-line)
      (next-line)
      (next-line)
      (org-table-export (format "%s.csv" name+org-time-stamp) "orgtbl-to-csv")))))

;(setq cua-mode t)
(setq fill-column 99999)
(setq paradox-automatically-star t)
(setq paradox-github-token "c8f68f39b767601a0af9df982990a68783c42642")
(setq send-mail-function (quote smtpmail-send-it))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(setq org-clock-clocked-in-display (quote mode-line))
(setq org-clock-idle-time nil)
(setq org-clock-out-remove-zero-time-clocks nil)
(setq org-clocktable-defaults
   (quote
    (:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :link nil :narrow 40! :indent t :timestamp nil :level nil :tcolumns nil :formatter nil :inherit-props t :emphasize t)))
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::")

(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq backup-directory-alist `(("." . "~/.saves")))

(setq org-alphabetical-lists t)
(setq org-ditaa-jar-path "~/org/Misc/ditaa.jar")
(setq org-plantuml-jar-path "~/org/Misc/plantuml.jar")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp  . t)
         (dot . t)
         (ditaa . t)
	 (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
	 ;;        (clojure . t)
	 (shell . t)
	 ;;        (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(setq org-babel-python-command "python3")
(setq org-html-inline-images t)
(setq org-export-with-sub-superscripts nil)
(setq org-html-head-include-default-style nil)
; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)
; Increase default number of headings to export
(setq org-export-headline-levels 6)

(setq org-export-allow-BIND t)
(setq org-export-allow-bind-keywords t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")
	      (sequence "DRAFT(k)" "|" "SENT(b@/!)" "PAID(p)")
	      (sequence "TODO(t)" "DRAFT(f@/!)" "FUTURE" "|""POSTED(o)")
        )))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("SOMEDAY" ("SOMEDAY" . t)))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(defun br/org-remove-empty-propert-drawers ()
  "*Remove all empty property drawers in current file."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "You need to turn on Org mode for this function."))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":ID:" nil t)
      (save-excursion
        (org-remove-empty-drawer-at "ID" (match-beginning 0))))))

(setq org-deadline-warning-days 0)
(setq org-fontify-done-headline t)
(setq org-footnote-auto-adjust t)
(setq org-footnote-auto-label (quote plain))
(setq org-hide-leading-stars t)
(setq org-icalendar-timezone "Asia/Calcutta")
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))
(setq org-log-reschedule (quote time))
(setq org-pandoc-epub-rights
   "Copyright Ã‚Â© 2016 Dr. Bala Ramadurai <bala@balaramadurai.net>")
(setq org-show-mode t)
(setq org-startup-truncated nil)
(setq org-support-shift-select t)
(setq org-tags-column -117)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/org/contrib/lisp"))
(with-eval-after-load 'org
  (require 'ox-extra)
  (require 'ox-bibtex)
  (ox-extras-activate '(ignore-headlines))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  )

(with-eval-after-load 'org
  (require 'org-tempo)
  (require 'org-habit)
  (require 'org-checklist)
  (require 'org-protocol)
  (add-to-list 'org-capture-templates
               '("l" "Protocol" entry (file "~/org/inbox.org")
                 "* %^{Title}\nSource: [[%:link][%:description]]\n%u, %c\n #+BEGIN_QUOTE\n%:initial\n#+END_QUOTE\n\n\n%?"))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry (file "~/org/inbox.org")
                 "* TODO %? \n SCHEDULED: %^{Time of Response?}T\n\n [[%:link][%:description]]\nCaptured On: %U"))
  )
(setq org-structure-template-alist
      (quote(("q" . "QUOTE")
	     ("v" . "VERSE")
	     ("muse" . "SRC emacs-lisp :tangle ~/.emacs.d/elisp/base-extensions.el \n(use-package ?\n\n\n :diminish\n; :general\n; :config\n)\n")
	     ("m" . "SRC emacs-lisp")
	     ("r" . "SRC R :results output :session *R* :exports both")
	     ("R" . "SRC R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R*")
	     ("RR" . "SRC R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R*")
	     ("p" . "SRC python :results output :exports both")
	     ("P" . "SRC python :results output :session :exports both")
	     ("PP" . "SRC python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename")
	     )))

)

(use-package org-re-reveal
   :after org
   :config
   (setq org-reveal-external-plugins
   (quote
    ((menu . "{src: '%splugin/menu/menu.js'}")
     (toolbar . "{src: '%splugin/toolbar/toolbar.js'}")
     (jump . "{ src: '%splugin/jump/jump.js', async: true }")
     (zoom-js . "{ src: '%splugin/zoom-js/zoom.js', async: true }"))))
 )

(use-package org-clock-convenience

  :general
  (general-def org-agenda-mode-map
    "<S-up>"   'org-clock-convenience-timestamp-up
    "<S-down>" 'org-clock-convenience-timestamp-down))

(use-package org-pomodoro
  :general
  (spacemacs-lite/set-leader-keys-for-major-mode    "p"    'org-pomodoro)
 :config
 (setq org-pomodoro-length 45)
 (setq org-pomodoro-long-break-frequency 3)
 (setq org-pomodoro-long-break-length 30)
 (setq org-pomodoro-short-break-length 5)
 (setq org-pomodoro-ticking-sound-p t)
 (setq org-pomodoro-ticking-sound-states (quote (:short-break :long-break))))

(use-package org-present
  :defer t
  :general
  (general-def 'normal 'org-present-mode-keymap
    "h" 'org-present-prev
    "l" 'org-present-next
    "q" 'org-present-quit)
  :config
  (defun spacemacs-lite//org-present-start ()
    "Initiate `org-present' mode"
    (evil-emacs-state)
    (org-present-big)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only))
  (defun spacemacs-lite//org-present-end ()
    "Terminate `org-present' mode"
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (evil-normal-state))
  :hook
  ((org-present-mode-hook . spacemacs-lite//org-present-start)
  (org-present-mode-quit-hook . spacemacs-lite//org-present-end)))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("(#I)" "(#II)" "(#III)" "(#IV)")))

(use-package ox-hugo
  :config
  (setq org-export-with-author nil)
  ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	     (fname (org-hugo-slug title)))
	(mapconcat #'identity
		   `(
		     ,(concat "* TODO " title)
		     ":PROPERTIES:"
		     ,(concat ":EXPORT_FILE_NAME: " fname)
		     ":END:"
		     "%?\n")          ;Place the cursor here finally
		   "\n")))

    (add-to-list 'org-capture-templates
		 '("h"                ;`org-capture' binding + h
		   "Hugo post"
		   entry
		   ;; It is assumed that below file is present in `org-directory'
		   ;; and that it has a "Blog Post Ideas" heading. It can even be a
		   ;; symlink pointing to the actual location of all-posts.org!
		   (file+olp "balaramadurai.net.org" "Blog Post Ideas")
		   (function org-hugo-new-subtree-post-capture-template))))
  )

(use-package org-tracktable

; :diminish
; :general
; :config
)

(provide 'base-org)
;;; base-org ends here
