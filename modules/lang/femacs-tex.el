;;; femacs-tex.el --- Configuration for LaTeX mode
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configures Latex with Auctex, helm-bibtex and other packages.

;;LaTeX is a typesetting language which can be used for writing letters, reports and
;;manuscripts.  AucTeX servers as the fundamental mode for editing latex
;;documents.  Additionally, following features are provided
;;
;; - Auto-completion of commands and math markers.
;; - Auto-insertion of relevant citations and references via RefTeX.
;; - Use LaTeXMK to do all stages of compilation auto-magically.
;; - Helm-bibtex and ebib for managing bibliographies.

;;; License:

;; Copyright (c) 2016 Anurag Mishra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

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

;; Set variables first before loading modes
(defvar emacs-fast/tex-mode-backends
  '(company-files
    (company-math-symbols-latex company-latex-commands)
    company-capf
    company-dabbrev)
  "Company mode backends in LaTeX mode.")

(defun emacs-fast/latex-mode-hook ()
  "Settings for LaTeX mode."
  (setq fill-column 90)
  (setq whitespace-line-column 90)
  (setq-local company-backends emacs-fast/tex-mode-backends)
  (visual-line-mode 1)
  (latex-extra-mode)
  (auto-fill-mode 1)
  (company-mode 1)
  (nlinum-mode 1)
  (turn-on-reftex)
  (prettify-symbols-mode)
  (auctex-latexmk-setup)
  (TeX-source-correlate-mode))

;; Add auctex for editing
(use-package latex
  :ensure auctex
  :commands (TeX-mode LaTeX-mode LaTeX-mode-hook math-minor-mode)
  :mode     ("\\.[tT]e[xX]\\'" . LaTeX-mode)
  :bind (:map LaTeX-mode-map
              ("`e" . LaTeX-environment)
              ("`s" . LaTeX-section)
              ("`r" . reftex-reference)
              ("`c" . reftex-citation))
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-insert-braces nil)
  (add-hook 'LaTeX-mode-hook 'emacs-fast/latex-mode-hook))

;;;; ---------------- Other packages to manage LaTeX related stuff -----------------------

(use-package helm-bibtex
  :ensure t
  :commands (helm-bibtex)
  :init
  (defcustom bibtex-completion-bibliography nil
    "List of Bibtex files."
    :type '(repeat file)
    :group 'helm-bibtex))

(use-package ebib
  :ensure t
  :commands (ebib))

(provide 'femacs-tex)
;;; femacs-tex.el ends here
