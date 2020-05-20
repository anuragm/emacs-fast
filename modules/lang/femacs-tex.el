;;; femacs-tex.el --- Configuration for LaTeX mode
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configures Latex with Auctex, helm-bibtex and other packages.

;; LaTeX is a typesetting language which can be used for writing letters, reports and
;; manuscripts.  AucTeX serves as the fundamental mode for editing latex
;; documents.  Additionally, following features are provided
;;
;; - Auto-completion of commands and math markers.
;; - Auto-insertion of relevant citations and references via RefTeX.
;; - Use LaTeXMK to do all stages of compilation auto-magically.
;; - Helm-bibtex and ebib for managing bibliographies.

;; Copyright (c) 2016-2018 Anurag Mishra, MIT License.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:


;; Helper packages

;; Compile with Latexmk
(use-package auctex-latexmk
  :ensure t
  :commands (auctex-latexmk-setup))

;; Add math symbols help to latex mode, and expand commands (like yasnippet).
(use-package cdlatex
  :ensure t
  :commands (cdlatex-mode turn-on-cdlatex)
  :diminish cdlatex-mode)

;; And company math mode for auto-complete
(use-package company-math
  :ensure t
  :commands (company-math-symbols-latex
             company-math-latex-commands))

;; Use latex extra for better indentation and auto fill.
(use-package latex-extra
  :ensure t
  :custom (latex/override-font-map nil "Don't hijack C-c f")
  :commands (latex-extra-mode)
  :diminish latex-extra-mode)

;; Magic latex buffer, for better latex font lock and preview.
(use-package magic-latex-buffer
  :ensure t
  :commands (magic-latex-buffer))

;; Reftex comes packaged with Emacs > 20.2
(use-package reftex
  :ensure nil
  :config
  (diminish 'reftex-mode "ⓡ"))


;; Use company auctex for better completion.

(use-package company-auctex
  :ensure t
  :commands
  (company-auctex-labels company-auctex-bibs company-auctex-macros
   company-auctex-symbols company-auctex-environments))

;; Use company reftex for citations and reference
(use-package company-reftex
  :ensure t
  :commands (company-reftex-labels company-reftex-citations))

;; Set variables first before loading modes
(defvar emacs-fast/tex-mode-backends
  '(company-reftex-labels company-reftex-citations
    (company-math-symbols-latex company-latex-commands)
    (company-auctex-macros company-auctex-symbols company-auctex-environments)
    company-dabbrev company-files company-capf)
  "Company mode backends in LaTeX mode.")


;; Add support for clever ref package to Reftex.

(defun emacs-fast/add-cref-support()
  "Add support for Cref labels to TeX mode."
  (TeX-add-style-hook
   "cleveref"
   (lambda ()
     (if (boundp 'reftex-ref-style-alist)
         (add-to-list
          'reftex-ref-style-alist
          '("Cleveref" "cleveref"
            (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
     (reftex-ref-style-activate "Cleveref")
     (TeX-add-symbols
      '("cref" TeX-arg-ref)
      '("Cref" TeX-arg-ref)
      '("cpageref" TeX-arg-ref)
      '("Cpageref" TeX-arg-ref)))))

(defvar font-latex-match-reference-keywords)
(setq font-latex-match-reference-keywords
      '(
        ;; cleveref
        ("cref" "{")
        ("Cref" "{")
        ("cpageref" "{")
        ("Cpageref" "{")
        ("cpagerefrange" "{")
        ("Cpagerefrange" "{")
        ("crefrange" "{")
        ("Crefrange" "{")
        ("labelcref" "{")))


;; Add additional keyword highlighting.
;; Taken from tex.stackexchange.com/questions/85849/

(defvar font-latex-match-textual-keywords)
(setq font-latex-match-textual-keywords
      '(
        ;; subcaption
        ("subcaption" "[{")))

(defvar font-latex-match-variable-keywords)
(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")
        ("crefname" "{")))


;; AUCTeX for editing

(defun emacs-fast/latex-mode-hook ()
  "Settings for LaTeX mode."
  (setq fill-column 90)
  (setq-local company-backends emacs-fast/tex-mode-backends)
  (rainbow-delimiters-mode-enable)
  (visual-line-mode 1)
  (latex-extra-mode)
  (auto-fill-mode 1)
  (company-mode 1)
  (nlinum-mode 1)
  (turn-on-reftex)
  (prettify-symbols-mode)
  (whitespace-mode)
  (auctex-latexmk-setup)
  (TeX-source-correlate-mode)
  (diff-hl-mode))

(use-package latex
  :ensure auctex
  :commands (TeX-mode LaTeX-mode LaTeX-mode-hook math-minor-mode)
  :mode     (("\\.[tT]e[xX]\\'" . LaTeX-mode)
             ("\\.tikz\\'" . LaTeX-mode))
  :bind (:map LaTeX-mode-map
              ("`e" . LaTeX-environment)
              ("`s" . LaTeX-section)
              ("`r" . reftex-reference)
              ("`c" . reftex-citation))
  :functions (TeX-add-style-hook TeX-add-symbols TeX-source-correlate-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (reftex-plug-into-AUCTeX t)
  (TeX-PDF-mode t)
  (TeX-electric-sub-and-superscript t)
  (TeX-insert-braces nil)
  (TeX-electric-math '("$" . "$"))
  (TeX-auto-local ".auto") ;; Store parsed information in .auto directory.
  :init
  (add-hook 'LaTeX-mode-hook 'emacs-fast/latex-mode-hook)
  :config
  (emacs-fast/add-cref-support))


;;;; Other packages to manage LaTeX related stuff.

(use-package helm-bibtex
  :ensure t
  :commands (helm-bibtex)
  :init
  (setq bibtex-completion-pdf-field "File"))

(when (version< "25.3" emacs-version)
  (use-package ebib
  :ensure t
  :commands (ebib)))


;; Functions for useful transformation.

;; Convert from Subfigure to subcaption package.
(defun femacs/subfigure-to-subcaption ()
  "Convert subfigure package notation to subcaption package notations."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
     "^\s*\\\\subfigure\\[\\(.*?\\)]{.*\\includegraphics\\[width=\\(.+?\\)]{\\(.+?\\)}\\(.*?}?\\)}"
          nil t)
    (replace-match
     "\\\\begin{subfigure}[t]{\\2}
 \\\\includegraphics[width=\\\\columnwidth]{\\3}
 \\\\caption{\\1}
 \\4
\\\\end{subfigure}")))

(provide 'femacs-tex)
;;; femacs-tex.el ends here
