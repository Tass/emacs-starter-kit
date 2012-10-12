;; Author: Simon Hafner hafnersimon@gmail.com
;; This code is public domain.

(require 'org)

(defun org-babel-execute:qtree (body params)
  "Reformat a block of lisp-edited tree to one tikz-qtree likes."
  (let (( tree
          (concat "\\begin{tikzpicture}
\\tikzset{every tree node/.style={align=center, anchor=north}}
\\Tree "
                  (replace-between-words
                   (replace-regexp-in-string
                    (regexp-quote "]") " ]" ; qtree needs a space
                                        ; before every closing
                                        ; bracket.
                    (replace-regexp-in-string
                     (regexp-quote "[]") "[.{}]" body)) ; empty tree
                                                        ; nodes, see
                                                        ; http://tex.stackexchange.com/questions/75915
                   "\\\\") ; For
                           ; http://tex.stackexchange.com/questions/75217
                  "\n\\end{tikzpicture}"
                  )))
    (if (assoc :landscape params)
        (concat "\\begin{landscape}\n" tree "\n\\end{landscape}")
      tree)))

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

(defun replace-between-words (input replacement &optional preserve)
  "Replaces white space between two words with REPLACEMENT
if REPLACEMENT is a character, and PRESERVE is not NIL, then
that character is duplicated as many times as there are white spaces
between words. If PRESERVE is NIL, then only one character is
inserted.
If REPLACEMENT is a string and PRESERVE is not NIL, then it is rolled
into the available white space, otherwise the entire replacement string
is insterted.
http://stackoverflow.com/questions/12809610/replace-regexp-in-string-with-lookahead-behind/12811460"
  (with-output-to-string
    (let ((match "*") (replaced t)
          (white-count 0)
          seen-start seen-end current whites)
      (dotimes (i (length input))
        (setf current (aref input i)
              (aref match 0) current)
        (cond
         ((string-match "\\w" match)
          (if seen-end
              (progn
                (if (stringp replacement)
                    (if preserve
                        (dotimes (j white-count)
                          (write-char
                           (aref replacement
                                 (mod j (length replacement )))))
                      (princ replacement))
                  (if preserve
                      (dotimes (j white-count)
                        (write-char replacement))
                    (write-char replacement)))
                (setq seen-end nil))
            (setq seen-start t))
          (setq whites nil white-count 0)
          (write-char current))
         ((member current '(?\ ?\t ?\n ?\r))
          (if seen-start
              (if seen-end
                  (progn
                    (setq whites (cons current whites))
                    (incf white-count))
                (setq seen-end t white-count 1 whites (list ?\ )))
            (write-char current)))
         (t (when (> white-count 0)
              (princ (coerce whites 'string))
              (setq white-count 0 whites nil))
            (write-char current)
            (setq seen-end nil seen-start nil)))))))

(provide 'org-babel-qtree)
