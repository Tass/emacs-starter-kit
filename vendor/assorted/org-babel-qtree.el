;; Author: Simon Hafner hafnersimon@gmail.com
;; This code is public domain.

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(defvar org-babel-qtree-default-functions '("space")
"Available: \"space\", \"empty-leaves\".")

(defun org-babel-execute:qtree (body params)
  "Reformat a block of lisp-edited tree to one tikz-qtree likes."
  (let ((tree
          (concat "\\begin{tikzpicture}
\\tikzset{every tree node/.style={align=center, anchor=north}}
\\Tree " (org-babel:qtree-process-string body (or (split-string (or (cdr (assoc :qtree params)) "") " " t) org-babel-qtree-default-functions))
                  "\n\\end{tikzpicture}"
                  )))
    (if (assoc :landscape params)
        (concat "\\begin{landscape}\n" tree "\n\\end{landscape}")
      tree)))

(defun org-babel:qtree-process-string (string functions-to-call)
  (apply 'concat
   (loop for line in (split-string string "\n")
         if (not (string-match "^[\\s\\t]*\\\\" line))
         collect (org-babel:qtree-modify-tree functions-to-call line)
         else collect (concat line "\n")
         )))

(defun org-babel:qtree:space (line)
  "qtree needs a space before every closing bracket."
  (message (pp line))
  (replace-regexp-in-string
   (regexp-quote "]") " ]" line)
  line)

(defun org-babel:qtree:empty-leaves (line)
  "empty leaf nodes, see
http://tex.stackexchange.com/questions/75915
http://tex.stackexchange.com/questions/75217"
  (replace-regexp-in-string
   (regexp-quote "[]") "[.{}]" line)
  line)

(defun org-babel:qtree-modify-tree (functions-to-call line)
  "This function modifies a line with the listed functions in
  order. The function names are created with (concat
  org-babel:qtree:<name>)"
  (reduce (lambda (previous fun-name) (funcall (intern (concat "org-babel:qtree:" fun-name)) previous)) functions-to-call :initial-value line))

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
