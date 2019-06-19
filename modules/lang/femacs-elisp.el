;;; femacs-elisp.el --- Support for editing Emacs lisp files.
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds useful modes and enables fly-check

;;; License:

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

;; Display form feed character (^L) as a line.
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :commands (page-break-lines-mode))

;; CAUTION : Set variables first before loading modes.
(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (unless (string= "*scratch*" (buffer-name))
      (setq-local fill-column 90)
      (setq-local whitespace-line-column 90)
      (electric-pair-local-mode)
      (auto-fill-mode)
      (flycheck-mode 1)
      (yas-minor-mode 1)
      (whitespace-mode)
      (company-mode)
      (page-break-lines-mode)
      (eldoc-mode))))

(provide 'femacs-elisp)
;;; femacs-elisp.el ends here
