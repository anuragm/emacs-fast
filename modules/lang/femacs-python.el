;;; femacs-python.el --- Support for Python programming
;;
;; Copyright © 2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds support for Python programming language using auto completion, REPL, etc.

;;; License:

;; Copyright (c) 2018 Anurag Mishra

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

;; Add iPython support from http://millejoh.github.io/emacs-ipython-notebook/
;; Ensure that iPython and Jupyter binaries are in Emacs path.
(use-package ein
  :ensure t
  :pin melpa-stable
  :diminish ein:notebook-mode
  :commands
  (ein:jupyter-server-start ein:notebooklist-login ein:login)
  :config
  (require 'ein-notebook)
  (require 'ein-subpackages))

(defun femacs/python-mode-hook()
  "Custom bindings for python mode."
  (setq-local fill-column 90)
  (electric-indent-mode)
  (nlinum-mode)
  (auto-fill-mode)
  (company-mode)
  (whitespace-mode)
  (dtrt-indent-mode))

(add-hook 'python-mode-hook #'femacs/python-mode-hook)

;; Use ELPY for Python programming. https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :diminish "")

(provide 'femacs-python)
;;; femacs-python.el ends here
