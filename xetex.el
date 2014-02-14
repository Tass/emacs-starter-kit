;;; XeLaTeX customisations
;; remove "inputenc" from default packages as it clashes with xelatex
(setf org-export-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

(add-to-list 'org-export-latex-packages-alist '("" "xltxtra" t))
;; choose Linux Libertine O as serif and Linux Biolinum O as sans-serif fonts
(add-to-list 'org-export-latex-packages-alist '("" "libertineotf" t))

;; org to latex customisations, -shell-escape needed for minted
(setq org-export-dispatch-use-expert-ui t ; non-intrusive export dispatch
      org-latex-pdf-process               ; for regular export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
