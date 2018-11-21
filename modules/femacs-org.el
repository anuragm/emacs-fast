;;; femacs-org.el --- Org mode configuration for Fast emacs
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains some useful packages related to org-mode, such as org-ref that adds citation
;; capabilities, etc.

;;; License:

;; Copyright (c) 2016 Anurag Mishra, MIT License.

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
;;(use-package org-ref
;;  :ensure t
;;  :commands (org-ref org-ref-helm-cite)
;;  :defer t
;;  )

;; Make org-ref useful for HTML output.
;;(use-package org-ref-citeproc
;;  :ensure nil
;;  :after org-ref
;;  :config
;;  (let ((org-export-before-parsing-hook '(orcp-citeproc)))
;;    (org-open-file (org-org-export-to-org)))
;;  (add-hook 'org-export-before-parsing-hook 'orcp-citeproc))

(defun femacs/org-mode-hook ()
  "Custom hook for ORG MODE."
  (setq-local fill-column 95)
  (setq-local whitespace-line-column 95)
  (auto-fill-mode))

(use-package org
  :ensure org-plus-contrib ;; Download newer Org and extra pacakges.
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-src-tab-acts-natively t) ;; Indent naturally in code blocks
  (add-hook 'org-mode-hook 'femacs/org-mode-hook)
  :config
  (add-to-list 'org-export-backends 'md) ;; Add Markdown to export back ends.
  (require 'ox-bibtex)) ; Enable BibTex export in HTML

(use-package ox-slack ;; Add slack exporter for Org mode.
  :quelpa (ox-slack :fetcher github :repo "titaniumbones/ox-slack"))

(provide 'femacs-org)
;;; femacs-org.el ends here
