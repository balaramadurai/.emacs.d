;; *****************
;; Personal Information
   (setq user-full-name "Bala Ramadurai"
      user-mail-address "bala@balaramadurai.net")
   (setq org-agenda-files '("~/org/index.org" "~/org/writing.org" "~/org/teaching.org" "~/org/learning.org" "~/org/marketing.org" "~/org/inbox.org"))
(defun my/phone-p ()
  (and (equal (system-name) "localhost") (not (equal user-login-name "bala"))))

(eval-and-compile
  (setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6))

;; Init time start
(defvar my-init-el-start-time (current-time) "Time when init.el was started")

(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 99999)		; toggle wrapping to a really long line
(defalias 'yes-or-no-p 'y-or-n-p) ; Simplify life
(setq ad-redefinition-action 'accept)         ; to get rid of the annoying "ad-handle-definition" warning
(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))
(set-face-attribute 'default nil :family "Iosevka" :height 95)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 95)
(set-face-attribute 'variable-pitch nil :family "Baskerville" :height 110)

					;  (flyspell-mode 1)        ;; Catch Spelling mistakes
(blink-cursor-mode 0)    ;; Reduce visual noise

;; These instructions are coming from this website - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;;
;; Packages

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)
(server-start)

(global-auto-revert-mode 1)
(setq frame-title-format "%b")

(setq browse-url-browser-function (quote browse-url-default-browser))

(unless (file-directory-p (concat private-dir "/backups"))
		       (make-directory (concat private-dir "/backups") :parents))
(setq backup-directory-alist '(("." . "~/.emacs.d/private/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/private/auto-save-list/" t)))
(unless (file-exists-p (concat private-dir "/cache/recentf"))
		       (make-directory (concat private-dir "/cache/") :parents))
(setq recentf-save-file "~/.emacs.d/private/cache/recentf")

(setq savehist-file "~/.emacs.d/private/cache/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(setq-default save-place t)

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

(provide 'base-emacs)
;;; base-org ends here
