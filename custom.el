(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium))
 '(company-backends
   (quote
    (company-nim company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-files company-dabbrev)))
 '(custom-safe-themes
   (quote
    ("9226bb420f414235005f1977615fd93cfbf486648c98c87f4c22f300e03f00a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(evil-shift-width 2)
 '(glasses-uncapitalize-p t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(nim-command "/home/tass/dev/nimrod/Nimrod/bin/nim")
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/.org-jira/vidcast.org" "~/.org-jira/DV.org")))
 '(org-export-latex-inline-image-extensions (quote ("pdf" "jpeg" "jpg" "png" "ps" "eps" "svg")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "okular %s"))))
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-tree-slide-slide-in-effect nil)
 '(python-indent-offset 2)
 '(send-mail-function nil)
 '(shell-prompt-pattern "^[^#$%>
]*[#$%>~] *")
 '(show-paren-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray14"))))
 '(paren-face-match ((t (:background "orange red" :foreground "white")))))
