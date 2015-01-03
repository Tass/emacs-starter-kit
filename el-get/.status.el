((Emacs-Groovy-Mode status "installed" recipe
                    (:name Emacs-Groovy-Mode :type github :pkgname "russel/Emacs-Groovy-Mode" :description "This is a collection of (X)Emacs modes for use with Groovy-related technology -- Groovy, Grails, etc."))
 (ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs." :type github :pkgname "winterTTr/ace-jump-mode"))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)
                       :features auto-complete-config :post-init
                       (progn
                         (add-to-list 'ac-dictionary-directories
                                      (expand-file-name "dict" default-directory))
                         (ac-config-default))))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (ensime status "installed" recipe
         (:name ensime :description "ENhanced Scala Interaction Mode for Emacs" :type github :pkgname "ensime/ensime-emacs" :depends
                (s dash popup auto-complete scala-mode2)
                :prepare
                (progn
                  (autoload 'ensime-scala-mode-hook "ensime")
                  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))))
 (festival status "installed" recipe
           (:name festival :auto-generated t :type emacswiki :description "emacs interface into festival." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/festival.el"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (haskell-mode status "installed" recipe
               (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :info "." :build
                      `(("make" ,(format "EMACS=%s" el-get-emacs)
                         "all"))
                      :post-init
                      (progn
                        (require 'haskell-mode-autoloads)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (newpastebin status "installed" recipe
              (:name newpastebin :auto-generated t :type emacswiki :description "" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/newpastebin.el"))
 (org-mode status "installed" recipe
           (:name org-mode :website "http://orgmode.org/" :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system." :type git :url "git://orgmode.org/org-mode.git" :info "doc" :build/berkeley-unix `,(mapcar
                                                                                                                                                                                                                                                                                                       (lambda
                                                                                                                                                                                                                                                                                                         (target)
                                                                                                                                                                                                                                                                                                         (list "gmake" target
                                                                                                                                                                                                                                                                                                               (concat "EMACS="
                                                                                                                                                                                                                                                                                                                       (shell-quote-argument el-get-emacs))))
                                                                                                                                                                                                                                                                                                       '("oldorg"))
                  :build `,(mapcar
                            (lambda
                              (target)
                              (list "make" target
                                    (concat "EMACS="
                                            (shell-quote-argument el-get-emacs))))
                            '("oldorg"))
                  :load-path
                  ("." "contrib/lisp" "lisp")
                  :load
                  ("lisp/org-loaddefs.el")))
 (org-reveal status "installed" recipe
             (:name org-reveal :type github :pkgname "yjwen/org-reveal" :description "Exports Org-mode contents to Reveal.js HTML presentation." :depends org-mode :features ox-reveal))
 (org-tree-slide status "installed" recipe
                 (:name org-tree-slide :description "A presentation tool for org-mode" :type github :pkgname "takaxp/org-tree-slide" :required nil))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (pymacs status "installed" recipe
         (:name pymacs :description "Interface between Emacs Lisp and Python" :type github :pkgname "pinard/Pymacs" :prepare
                (progn
                  (el-get-envpath-prepend "PYTHONPATH" default-directory)
                  (autoload 'pymacs-load "pymacs" nil t)
                  (autoload 'pymacs-eval "pymacs" nil t)
                  (autoload 'pymacs-exec "pymacs" nil t)
                  (autoload 'pymacs-call "pymacs")
                  (autoload 'pymacs-apply "pymacs"))
                :build
                ("make")))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el"))
 (scala-mode2 status "installed" recipe
              (:name scala-mode2 :description "A new scala-mode for Emacs 24." :type github :pkgname "hvesalai/scala-mode2")))
