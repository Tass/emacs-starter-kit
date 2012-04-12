;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(setq vendor-dir (concat dotfiles-dir "vendor/"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path))

(require 'icicles)

(require 'nimrod-mode)

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup)

(require 'evil)
(evil-mode 1)
(setq evil-auto-indent 't)
(setq evil-shift-width 2)
(define-key evil-insert-state-map (kbd "RET") 'reindent-then-newline-and-indent) 

(add-hook 'python-mode-hook
  (function (lambda ()
          (setq evil-shift-width python-indent))))
(add-hook 'ruby-mode-hook
  (function (lambda ()
          (setq evil-shift-width ruby-indent-level))))

(require 'undo-tree)
(global-undo-tree-mode)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(require 'ecb)

(require 'gist)

(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))

(require 'tramp)

(require 'edit-server)
(edit-server-start)

; r-mode
(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(require 'ess-site)

; assorted stuff
(require 'tempo-snippets)

(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

; Stuff for window management

(defun detach-window () (interactive) (let ((new-frame (save-excursion(make-frame-command)))) (delete-window) (select-frame new-frame)))
(global-set-key (kbd "C-x 2") 'detach-window)

; set frame title so the window manager can read it
(setq frame-title-format
  '(:eval (or (buffer-file-name (current-buffer)) "#<none>")))

; block C-x C-c
(defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
  "Disable C-x C-c under X."
  (if (or (eq window-system 'x) (eq window-system 'w32))
      (message "I'm afraid I can't do that.")
    ad-do-it))
(ad-activate 'save-buffers-kill-terminal)

(define-key lisp-interaction-mode-map (kbd "C-c x") 'le::eval-and-insert-results)
(setq show-paren-style 'expression)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
(yas/load-directory (concat dotfiles-dir "snippets"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat vendor-dir "auto-complete/dict"))
(ac-config-default)

;;; init.el ends here
