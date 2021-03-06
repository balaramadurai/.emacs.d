(use-package diminish

  :config
  (diminish 'eldoc-mode "")
  (diminish 'buffer-face-mode "")
  (diminish 'undo-tree-mode " Ⓤ")
)

;; General package
(use-package general

;  :after which-key
  :config
  (general-override-mode 1)

   (general-create-definer spacemacs-lite/set-leader-keys
    :states '(normal visual motion emacs)
    :prefix "SPC")

    (general-create-definer spacemacs-lite/set-leader-keys-for-major-mode
    :states '(normal emacs)
    :prefix ".")

    (general-define-key
    :keymaps 'key-translation-map
    "ESC" (kbd "C-g"))

    (general-def
    "<f2>"             'org-agenda
    "M-]"              'next-buffer
    "M-["              'previous-buffer
    "C-+"              'text-scale-increase
    "C--"              'text-scale-decrease
    )

    (spacemacs-lite/set-leader-keys-for-major-mode
    ""      '(nil :which-key "Org helper"))

    (spacemacs-lite/set-leader-keys
    ""     '(nil :which-key "Spacemacs-Lite")
    "a"    '(:ignore t :which-key "apps")
    "b"    '(:ignore t :which-key "buffer")
    "c"    '(:ignore t :which-key "comments")
    "f"    '(:ignore t :which-key "files")
    "g"    '(:ignore t :which-key "git")
    "h"    (general-simulate-key "C-h" :which-key "help")
    "i"    '(:ignore t :which-key "insert")
    "m"    '(:ignore t :which-key "modes")
    "P"    '(:ignore t :which-key "Packages")
    "q"    '(:ignore t :which-key "quit")
    "s"    '(:ignore t :which-key "search")
    "u"    (general-simulate-key "C-u" :which-key "universal")
    "w"    '(:ignore t :which-key "window")
    "x"    '(:ignore t :which-key "text")
    "xg"   '(:ignore t :which-key "google-translate")
    "xw"   '(:ignore t :which-key "words")

    ;; Applications
    "ad"   'dired
;    "ac"   'calendar
    "as"   'ansi-term

    ":"    'shell-command

    ;; buffer management
    ;; "bb"   'switch-to-buffer
    "b]"   'next-buffer
    "b["   'previous-buffer
    "ba"   'copy-whole-buffer-to-clipboard
    "bc"   'write-file
    "bd"   'kill-this-buffer
    "bD"   'spacemacs-lite/kill-other-buffers
    "bR"   'rename-file-and-buffer
    "br"   'revert-buffer
    "bm"   'show-messages-buffer
    "bh"   'show-home-buffer
    "bs"   'show-scratch-buffer
    "bY"   'copy-whole-buffer-to-clipboard
    "TAB"  '(mode-line-other-buffer :wk "last buffer")

    ;; Comments
    "cl"   'comment-or-uncomment-line
    "cr"   'comment-region

    ;; file operations
    ;; "ff"   'find-file
    "fc"   '(spacemacs-lite/copy-file :wk "copy-file")
    "fD"   '(spacemacs-lite/delete-current-buffer-file :wk "delete-file")
    "fe"   '(:ignore t :which-key "emacs")
    "fE"   '(spacemacs-lite/sudo-edit :wk "sudo-edit")
    "fed"  'find-user-init-file
    "feR"  'load-user-init-file
    "fec"  'find-user-config-org-file
    "fo"   '(spacemacs-lite/open-file-or-directory-in-external-app :wk "open-in-ext")
    "fR"   '(spacemacs-lite/rename-current-buffer-file :wk "rename-file")
    "fs"   'save-buffer

    ;; help

    ;; package manager
    "Pr"   'package-autoremove
    "Pd"   'package-delete
    "Pl"   'list-packages
    "Pi"   'package-install
    "Pu"   'package-upgrade-all

    ;; quit emacs
    "qq"   'kill-emacs

    ;; window management
    "wm"   'delete-other-windows
    "w/"   'split-window-horizontally
    "w-"   'split-window-vertically
    "wd"   'delete-window

    )

    (general-def 'normal doc-view-mode-map
      "/"   'isearch-forward)

    (general-def 'normal package-menu-mode-map
      "i"   'package-menu-mark-install
      "U"   'package-menu-mark-upgrades
      "d"   'package-menu-mark-delete
      "u"   'package-menu-mark-unmark
      "x"   'package-menu-execute
      "q"   'quit-window)

    (general-def 'normal term-mode-map
      "i"   'package-menu-mark-install
      "U"   'package-menu-mark-upgrades
      "d"   'package-menu-mark-delete
      "u"   'package-menu-mark-unmark
      "x"   'package-menu-execute
      "q"   'quit-window)

  )

;; Which-Key
(use-package which-key

  :diminish (which-key-mode . " Ⓚ")
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-idle-delay 0.25)
  (setq which-key-echo-keystrokes 0.18)
  )

(use-package ivy
  :diminish (ivy-mode . "")
  :general
  (spacemacs-lite/set-leader-keys
   "fr" 'counsel-recentf
   "rl" 'ivy-resume
   "bb" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq-default ivy-initial-inputs-alist nil)
  (setq ivy-count-format ""
       ivy-display-style nil
       ivy-fixed-height-minibuffer t
       ivy-height 20
       ivy-re-builders-alist '((t . ivy--regex-plus))
       ivy-format-functions-alist '((t . ivy-format-function-line)))
  )


(setq ivy-use-virtual-buffers t)

;; Example setting for ivy-views
(setq ivy-views
      `(("config {}"
         (vert
          (file "~/.emacs.d/README.org")
          (file "~/.emacs.d/init.el")
          ))
        ("kdt {}"
         (horz
          (file "~/Google Drive/1 Projects/2020 Karmic Design Thinking/manuscript/KDT-orange.org")))))
          
(use-package counsel
  :diminish (counsel-mode . "")
  :general
  (spacemacs-lite/set-leader-keys
    "SPC"  'counsel-M-x
    "ff"   'counsel-find-file
    "fr"   'counsel-recentf
    "fL"  'counsel-locate
    ;; help
    "?"   'counsel-descbinds
    ;; insert
    "iu"  'counsel-unicode-char
    ;; jump
    ;; register/ring
    "ry"  'counsel-yank-pop
    ;; jumping
    "sj"  'counsel-imenu
    )
  (general-def 'emacs org-agenda-mode-map
    "<SPC><SPC>"  '(counsel-M-x :wk "M-x")
    "<SPC>bb"   'counsel-switch-buffer
    "<SPC>ff"   'counsel-find-files
    )
  :config
  (counsel-mode 1)
  )

(use-package swiper
  :general
  (spacemacs-lite/set-leader-keys "ss" 'me/swiper)
					;  (general-def '(normal visual emacs motion) "/" 'swiper)
  :config
  (defun me/swiper ()
    "`swiper' with string returned by `ivy-thing-at-point' as initial input."
    (interactive)
    (swiper (ivy-thing-at-point)))
  (setq swiper-goto-start-of-match t))

(use-package alda-mode
:after evil
)

;; backup settings
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(use-package beancount
    :load-path "~/.emacs.d/plugin"
)

(use-package company

  :diminish (company-mode . " ⓐ")
  :config
  (global-company-mode t))

(use-package evil

  :diminish (evil-mode . " ⓔ")
  :hook (after-init . evil-mode)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'magit-mode 'normal)
  (cua-mode 1)
  (setq doc-view-continuous t)
  :general
  (spacemacs-lite/set-leader-keys
    "bN"   'evil-buffer-new
    "fd"   'evil-save-and-close
    )
  )

(use-package evil-unimpaired

  :requires evil
;  https://www.github.com/syl20bnr/spacemacs/layers/+spacemacs/spacemacs-evil/local/evil-unimpaired/evil-unimpaired.el
  :load-path "~/.emacs.d/plugin"
					; :diminish
					; :general
					; :config
  )

(use-package evil-goggles
					; :diminish
					; :general
  :config
  (setq evil-goggles-pulse t) ;; default is to pulse when running in a graphic display
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode)
  )

(use-package evil-tutor

; :diminish
 :general
 (general-define-key
 :keymaps 'help-mode-map
 :which-key "evil-tutor"
 "T" 'evil-tutor)
 :config
 (setq evil-tutor-working-directory "/tmp")
)

(use-package evil-magit

; :diminish
; :general
; :config
)

(use-package fountain-mode

  :config
  (setq fountain-pages-show-in-mode-line (quote timer))
  (setq fountain-trans-suffix-list (quote ("TO:" "WITH:" "FADE OUT" "TO BLACK" "CUT TO:"))))

(use-package magit

  :general
  (spacemacs-lite/set-leader-keys
    "gs"   'magit-status
    "gc"   'magit-commit-create
    "gp"   'magit-push-other
    "gS"   'magit-stage-file
    "gl"   'magit-log-all
    )
  )

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "xreader" (file)))))

(use-package restart-emacs

 :config
  (defun spacemacs-lite/restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args)))
   :general
   (spacemacs-lite/set-leader-keys
     "qr"   'restart-emacs
     "qd"   '(spacemacs-lite/restart-emacs-debug-init :which-key "quit with debug-init")
     )
   )

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar +treemacs-git-mode 'simple
  "Type of git integration for `treemacs-git-mode'.
There are 3 possible values:
  1) `simple', which highlights only files based on their git status, and is
     slightly faster,
  2) `extended', which highlights both files and directories, but requires
     python,
  3) `deferred', same as extended, but highlights asynchronously.
This must be set before `treemacs' has loaded.")

(use-package treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        )
  
  :general
  (spacemacs-lite/set-leader-keys "at" 'treemacs
    "0"  'treemacs-select-window)
  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-wrap-around t)
  (when +treemacs-git-mode
    ;; If they aren't supported, fall back to simpler methods
    (when (and (memq +treemacs-git-mode '(deferred extended))
               (not (executable-find "python3")))
      (setq +treemacs-git-mode 'simple))
    (treemacs-git-mode +treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq treemacs-git-mode '(extended deferred))
              3
            0)))
        )

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-magit
  :after treemacs magit)

(use-package winum

:general
(spacemacs-lite/set-leader-keys "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3)
:config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode 1)
)

(use-package writing-mode
  :load-path "~/.emacs.d/plugin/writing-mode"
  :general
  (spacemacs-lite/set-leader-keys
    "m w"  'writing-mode
    )
  )

(defun br/org-export-as-pdf ()
  (interactive)
  (save-buffer)
  (org-latex-export-to-pdf))

(use-package zoom
  :config
  ;; Golden Ratio
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))

(use-package gnuplot
    :defer t
    :general
    (spacemacs-lite/set-leader-keys         "ap" '(org-plot/gnuplot :which-key "gnuplot")))

(use-package org-superstar
					; :general
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-special-todo-items t)
  )

(provide 'base-extensions)
;;; base-extensions ends here
