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
    "t"    '(:ignore t :which-key "themes")
    "u"    (general-simulate-key "C-u" :which-key "universal")
    "w"    '(:ignore t :which-key "window")
    "x"    '(:ignore t :which-key "text")
    "xg"   '(:ignore t :which-key "google-translate")
    "xw"   '(:ignore t :which-key "words")

    ;; Applications
    "ad"   'dired
;    "ac"   'calendar

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

    ;; Theme operations
    "tn"  'my/cycle-theme
    "tt"  'load-theme
    "tl"  'load-leuven-theme
    "td"  'load-dichromacy-theme
    "tp"  'load-poet-theme
    "ts"  '(:ignore t :wk "spacemacs themes")
    "tsd" 'load-spacemacs-dark-theme
    "tsl" 'load-spacemacs-light-theme

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

;; Helm
(use-package helm

  :diminish (helm-mode . " Ⓗ")
  :general
  (spacemacs-lite/set-leader-keys
    "SPC"  'helm-M-x
    "bb"   'helm-mini
    "ff"   'helm-find-files
    "fr"   'helm-recentf
    "ik"   'helm-show-kill-ring
    )
  (general-def 'emacs org-agenda-mode-map
    "<SPC><SPC>"  '(helm-M-x :wk "M-x")
    "<SPC>bb"   'helm-mini
    "<SPC>ff"   'helm-find-files
    )

  :config
  (helm-mode 1)
  ;; https://github.com/emacs-helm/helm/issues/2175 - for arrows to go back or forward in find files
  (customize-set-variable 'helm-ff-lynx-style-map t)

  ;; https://emacs.stackexchange.com/questions/33727/how-does-spacemacs-allow-tab-completion-in-helm
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  )

(use-package helm-swoop

:general
(spacemacs-lite/set-leader-keys "ss" 'helm-swoop)
(general-def '(normal visual emacs motion) "/" 'helm-swoop-without-pre-input)
)

(use-package helm-projectile
  :general
  (spacemacs-lite/set-leader-keys
    "p"    '(:ignore t :wk "projects")
    "pr"   '(helm-projectile-recentf :wk "recent projects")
    "pf"   '(helm-projectile-find-file :wk "files")
    "pd"   '(projectile-dired :wk "directory")
    )

)

(use-package helm-descbinds

; :diminish
 :general
 (spacemacs-lite/set-leader-keys "?" '(helm-descbinds :wk "show keybindings"))
 :config
 (setq helm-descbinds-window-style 'split)
 :hook helm-mode-hook
)

(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  ;; backup settings
  (backup-by-copying t "don't clobber symlinks")
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t "use versioned backups")
  :config
  (setq confirm-kill-processes nil))

(use-package beancount
    :load-path "~/.emacs.d/plugin"
)

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

(use-package ox-tufte


; :diminish
; :general
:config
(setq safe-local-variable-values
   (quote
    ((eval add-hook
	   (quote after-save-hook)
	   (quote org-latex-export-to-pdf)
	   nil t)
     (org-inline-image-overlays)
     (org-latex-caption-above)
     (org-hide-macro-markers . t)
     (org-fontify-quote-and-verse-blocks . t)
     (eval org-sbe "latex-link")
     (eval org-sbe "latex-opt-link")
     (eval org-sbe "jk-keywords")
     (eval org-sbe "pdf-process-bibtex")
     (eval org-sbe "ngz-nbsp")
     (eval org-sbe "latex-filter-footcites")
     (eval org-sbe "biblatex-cite-link")
     (eval org-sbe "biblatex-textcite-link")
     (eval org-sbe "biblatex-parencite-link")
     (eval org-sbe "biblatex-sidecite-link")
     (eval org-sbe "biblatex-multicite-link")
     (eval org-sbe "biblatex-footcite-link")
     (eval org-sbe "tufte-ebib-setup")
     (eval org-sbe "tufte-handout")
     (eval org-sbe "tufte-book")
     (eval org-sbe "user-entities")
     (eval require
	   (quote ox-tufte-latex)))))
)

(use-package ox-tufte-latex
  :load-path "~/.emacs.d/plugin/tufte-org-mode"
)

(use-package winum

:general
(spacemacs-lite/set-leader-keys "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3)
:config
(setq winum-auto-setup-mode-line nil)
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

(provide 'base-extensions)
;;; base-extensions ends here
