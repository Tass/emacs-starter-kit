;; Author: Simon Hafner hafnersimon@gmail.com
;; This code is public domain.

(require 'org)

(defun org-babel-execute:qtree (body params)
  "Reformat a block of lisp-edited tree to one tikz-qtree likes."
  (let ((tree
          (concat "\\begin{tikzpicture}
\\tikzset{every tree node/.style={align=center, anchor=north}}
\\Tree " (org-babel:qtree-process-string body)
                  "\n\\end{tikzpicture}"
                  )))
    (if (assoc :landscape params)
        (concat "\\begin{landscape}\n" tree "\n\\end{landscape}")
      tree)))

(defun org-babel:qtree-process-string (string)
  (with-output-to-string
    (loop for line in (split-string string "\n")
          if (not (string-match "^[\\s\\t]*\\\\" line))
          do (princ (concat (replace-regexp-in-string
                             " \\_<\\w+\\_>" (lambda (x) (concat "\\\\\\\\" (substring x 1))) 
                             (replace-regexp-in-string
                              (regexp-quote "]") " ]"             ; qtree needs a space
                                        ; before every closing
                                        ; bracket.
                              (replace-regexp-in-string
                               (regexp-quote "[]") "[.{}]" line)) ; empty leaf
                                        ; nodes, see
                                        ; http://tex.stackexchange.com/questions/75915
                             )
                                        ; http://tex.stackexchange.com/questions/75217
                     "\n"))
          else do (princ (concat line "\n"))
                    ))
  )

(setq org-babel-default-header-args:qtree '((:results . "latex") (:exports . "results")))
(add-to-list 'org-src-lang-modes '("qtree" . qtree))
(define-generic-mode 
    'qtree-mode                  ;; name of the mode to create
  '("%")                         ;; comments start with '%'
  '()                            ;; no keywords
  '(("[." . 'font-lock-operator) ;; some operators
    ("]" . 'font-lock-operator))
  '()                      ;; files for which to activate this mode 
  '(paredit-mode)          ;; other functions to call
  "A mode for qtree edits" ;; doc string for this mode
  )

(provide 'org-babel-qtree)
