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

(defun add-items-to-list (list-var elements &optional append compare-fn)
  (mapc
   (lambda (element) (add-to-list list-var element append compare-fn))
   elements))

(require 'package)
(add-items-to-list 'package-archives
                   '(
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.milkbox.net/packages/")
                     )
                   t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'el-get)
(el-get 'sync)

(setq vendor-dir (concat user-emacs-directory "vendor/"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'auto-mode-alist '("\.scala" . scala-mode))

(defun reactormonk-parent-directory (dir &optional count)
  (let ((parent-dir (unless (equal "/" dir)
                      (file-name-directory (directory-file-name dir)))))
    (if (and count (> count 0))
        (reactormonk-parent-directory parent-dir (- count 1))
      parent-dir)))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'nimrod-mode)

(require 'evil)
(evil-mode 1)
(setq evil-auto-indent 't)
(add-hook 'text-mode-hook
  (function (lambda () (define-key evil-insert-state-map (kbd "RET") 'newline))))

(add-hook 'python-mode-hook
  (function (lambda ()
          (setq evil-shift-width python-indent))))

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook
  (function (lambda ()
              (setq evil-shift-width enh-ruby-indent-level)
              (define-key enh-ruby-mode-map "\C-c-"    'enh-ruby-insert-end)
              )))
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

(require 'evil-paredit)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(require 'surround)
(global-surround-mode 1)

(define-key evil-normal-state-map "gs"
  (lambda ()
    (interactive)
    (call-interactively
     (or
      (lookup-key (symbol-value (intern (format "%s-map" major-mode))) (kbd "M-."))
      'find-tag))))


(defun cofi/surround-add-pair (trigger begin-or-fun &optional end)
  "Add a surround pair.
If `end' is nil `begin-or-fun' will be treated as a fun."
  (push (cons (if (stringp trigger)
                  (string-to-char trigger)
                trigger)
              (if end
                  (cons begin-or-fun end)
                begin-or-fun))
        surround-pairs-alist))

(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(add-to-hooks (lambda ()
                (cofi/surround-add-pair "`" "`"  "'"))
              '(emacs-lisp-mode-hook lisp-mode-hook))
(add-to-hooks (lambda ()
                (cofi/surround-add-pair "~" "``"  "``"))
              '(markdown-mode-hook rst-mode-hook python-mode-hook))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (cofi/surround-add-pair "~" "\\texttt{" "}")
                             (cofi/surround-add-pair "=" "\\verb=" "=")
                             (cofi/surround-add-pair "/" "\\emph{" "}")
                             (cofi/surround-add-pair "*" "\\textbf{" "}")))
(add-to-hooks (lambda ()
                (cofi/surround-add-pair "c" ":class:`" "`")
                (cofi/surround-add-pair "f" ":func:`" "`")
                (cofi/surround-add-pair "m" ":meth:`" "`")
                (cofi/surround-add-pair "a" ":attr:`" "`")
                (cofi/surround-add-pair "e" ":exc:`" "`"))
              '(rst-mode-hook python-mode-hook))

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

(require 'undo-tree)
(global-undo-tree-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(define-key evil-normal-state-map "^" 'delete-indentation)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(require 'gist)

(require 'autopair)

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

(global-set-key (kbd "C-x C-b") (kbd "C-x b"))

(require 'le-eval-and-insert-results)
(define-key lisp-interaction-mode-map (kbd "C-c x") 'le::eval-and-insert-results)

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

(require 'org-babel-qtree)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'custom t)

(global-auto-revert-mode)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "<XF86Back>") 'previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'next-buffer)

(add-to-list 'auto-mode-alist '("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" . markdown-mode))

(setq ac-trigger-key "TAB")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t) (R . t)))

(require 'kwin)

(require 'ox-reveal)

(load "xetex")

(require 'mic-paren)

(setq org-default-notes-file "~/gtd/notes.org")
(define-key global-map "\C-cc" 'org-capture)

(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-visual-state-map (kbd "RET") nil)
(define-key evil-normal-state-map (kbd "RET") nil)

(ido-mode 't)
(global-set-key (kbd "M-x") 'smex)
(require 'ido-hacks)

;; (set-face-attribute 'default nil :font "MonacoB")

;;; init.el ends here

(put 'erase-buffer 'disabled nil)
