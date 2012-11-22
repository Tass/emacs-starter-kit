;;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path etc.

(add-to-list 'load-path user-emacs-directory)

;; Load up ELPA, the package manager

(setq package-user-dir (concat user-emacs-directory "elpa"))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings starter-kit-js starter-kit-ruby gist haml-mode magit paredit pastie ruby-mode yasnippet-bundle)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq vendor-dir (concat user-emacs-directory "vendor/"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path))

(require 'icicles)

(require 'nimrod-mode)

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-sprof))

(require 'evil)
(evil-mode 1)
(setq evil-auto-indent 't)
(setq evil-shift-width 2)
(add-hook 'text-mode-hook
  (function (lambda () (define-key evil-insert-state-map (kbd "RET") 'newline))))

(add-hook 'python-mode-hook
  (function (lambda ()
          (setq evil-shift-width python-indent))))
(add-hook 'ruby-mode-hook
  (function (lambda ()
          (setq evil-shift-width ruby-indent-level))))

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require 'undo-tree)
(global-undo-tree-mode)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(require 'gist)

(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda () (ruby-end-mode)))
(require 'autopair)
(autopair-global-mode)

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

(require 'le-eval-and-insert-results)
(define-key lisp-interaction-mode-map (kbd "C-c x") 'le::eval-and-insert-results)
(setq show-paren-style 'expression)

(require 'yasnippet)
(yas/global-mode 1)
(setq yas/snippet-dirs '((concat user-emacs-directory "snippets")))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat vendor-dir "auto-complete/dict"))
(add-to-list 'ac-sources 'ac-source-yasnippet)
(ac-config-default)

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq-default tab-width 4)
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

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(set-face-attribute 'default nil :height 100)

(setq x-select-enable-primary t)

;; dot-mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" "graphviz dot mode." t)
(add-to-list 'auto-mode-alist '("\.dot" . graphviz-dot-mode))

;; store your autosaved files in your system's tmp dir
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'custom-color)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)

