;;; femacs-qsharp.el --- Support for Q# language
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Includes basic highlighting support only.  No auto complete available yet.

;;; License:

;; Copyright (c) 2016-2021 Anurag Mishra

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;; Add Q# mode
(defun femacs/qsharp-mode-hook ()
  "Custom hook for Q# mode."
  (setq-local fill-column 95)
  (yas-minor-mode)
  (linum-mode))

(use-package qsharp-mode
  :quelpa
  (qsharp-mode :fetcher github :repo "anuragm/emacs-qsharp-mode")
  :commands qsharp-mode
  :mode (("\\.qs$" . qsharp-mode))
  :config
  (add-hook 'qsharp-mode-hook #'femacs/qsharp-mode-hook))

(provide 'femacs-qsharp)
;;; femacs-qsharp.el ends here
