;;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path etc.

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Load up ELPA, the package manager

(setq package-user-dir (concat user-emacs-directory "elpa"))

(defun add-items-to-list (list-var elements &optional append compare-fn)
  (mapc
   (lambda (element) (add-to-list list-var element append compare-fn))
   elements))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(el-get 'sync)

(require 'package)
(add-items-to-list 'package-archives
                   '(
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.milkbox.net/packages/")
                     )
                   t)
(package-initialize)

(setq vendor-dir (concat user-emacs-directory "vendor/"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path))

(defun reactormonk-parent-directory (dir &optional count)
  (let ((parent-dir (unless (equal "/" dir)
                      (file-name-directory (directory-file-name dir)))))
    (if (and count (> count 0))
        (reactormonk-parent-directory parent-dir (- count 1))
      parent-dir)))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t) (R . t)))
  (setq org-default-notes-file "~/gtd/notes.org")
  (define-key global-map "\C-cc" 'org-capture)
  )

(use-package scala-mode2
  :config
  :mode "\\.scala$"
  )

(use-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  )

(use-package nim-mode)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-auto-indent 't)
  (add-hook 'text-mode-hook
            (function (lambda () (define-key evil-insert-state-map (kbd "RET") 'newline))))

  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq evil-shift-width python-indent))))

  (evil-set-initial-state "Ensime-Popup-Buffer" 'emacs)

  (define-key evil-normal-state-map (kbd "gt") 'find-tag)

;;; esc quits
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (add-hook 'post-command-hook
            '(lambda ()
               (modify-syntax-entry ?_ "w")))

  (define-key evil-normal-state-map "gs"
    (lambda ()
      (interactive)
      (call-interactively
       (or
        (lookup-key (symbol-value (intern (format "%s-map" major-mode))) (kbd "M-."))
        'find-tag))))

  (defun cofi/evil-cursor ()
    "Change cursor color according to evil-state."
    (let ((default "OliveDrab4")
          (cursor-colors '((insert . "dark orange")
                           (emacs  . "sienna")
                           (visual . "white"))))
      (setq cursor-type (if (eq evil-state 'visual)
                            'hollow
                          'bar))
      (set-cursor-color (def-assoc evil-state cursor-colors default))))
  (setq evil-default-cursor #'cofi/evil-cursor)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map (kbd "RET") nil)
  )

(use-package enh-ruby-mode
  :mode "\\.rb$"
  :interpreter "ruby"
  :config
  (add-hook 'enh-ruby-mode-hook
            (function (lambda ()
                        (setq evil-shift-width enh-ruby-indent-level)
                        (define-key enh-ruby-mode-map "\C-c-"    'enh-ruby-insert-end)
                        )))
  )

(use-package evil-paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package pkgbuild-mode
  :mode "/PKGBUILD$"
  )

(use-package gist)

(use-package autopair)

(use-package edit-server
  :config
  (edit-server-start)
  )

(use-package ess-site
  :load-path "/usr/share/emacs/site-lisp/ess"
  :commands R
  )

(use-package yasnippet
  :config
  (yas/global-mode 1)
  (setq yas/snippet-dirs '((concat user-emacs-directory "snippets")))
  )

(use-package auto-complete-config
  :config
  (add-to-list 'ac-dictionary-directories (concat vendor-dir "auto-complete/dict"))
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (ac-config-default)
  (setq ac-trigger-key "TAB")
  )

(use-package coffee-mode
  :mode "\.coffee$"
  :mode "Cakefile"
  :config
  (defun coffee-custom ()
    "coffee-mode-hook"

    ;; CoffeeScript uses two spaces.
    (make-local-variable 'tab-width)
    (set 'tab-width 2)

    ;; If you don't want your compiled files to be wrapped
    (setq coffee-args-compile '("-c" "--bare"))

    ;; *Messages* spam
    (setq coffee-debug-mode t)

    ;; Compile '.coffee' files on every save
    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t)))

  (add-hook 'coffee-mode-hook 'coffee-custom)
  )

(use-package markdown-mode
  :mode "\\.md$"
  :mode "stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt"
  )

(use-package graphviz-dot-mode
  :mode "\\.dot$"
  )

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places")
  )

(use-package kwin)

(use-package ox-reveal)

(use-package mic-paren)

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

(global-set-key (kbd "C-x C-b") (kbd "C-x b"))

(require 'le-eval-and-insert-results)
(define-key lisp-interaction-mode-map (kbd "C-c x") 'le::eval-and-insert-results)

(set-face-attribute 'default nil :height 100)

(setq x-select-enable-primary t)

;; store your autosaved files in your system's tmp dir
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'org-babel-qtree)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'custom t)

(global-auto-revert-mode)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "<XF86Back>") 'previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'next-buffer)

(load "xetex")

(ido-mode 't)
(global-set-key (kbd "M-x") 'smex)
(use-package ido-hacks)

(setq org-latex-listings 'minted)

(put 'erase-buffer 'disabled nil)

;; (set-face-attribute 'default nil :font "MonacoB")

;;; init.el ends here
