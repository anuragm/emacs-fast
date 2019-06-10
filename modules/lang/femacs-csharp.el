;;; femacs-csharp.el --- C# programming support
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds C# mode support, with autocomplete via ReSharper, etc.

;;; License:

;; Copyright (c) 2016 Anurag Mishra

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

(defun femacs/csharp-mode-hook ()
  "Custom hook for C# mode."
  (run-hooks 'prog-mode-hook)
  (yas-minor-mode)
  (company-mode)
  (auto-fill-mode)
  (electric-pair-local-mode 1)
  (setq-local fill-column 90)
  (setq-local whitespace-line-column 90)
  (setq-local c-basic-offset 2)) ;; Indent with two spaces.

(use-package csharp-mode
  :ensure t
  :commands csharp-mode
  :mode (("\\.cs$" . csharp-mode))
  :config
  (add-hook 'csharp-mode-hook #'femacs/csharp-mode-hook))

;; Editing CSharp project files.
(use-package csproj-mode
  :ensure t
  :commands csproj-mode)

(provide 'femacs-csharp)
;;; femacs-csharp.el ends here
