;;; femacs-python.el --- Support for Python programming
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds support for Python programming language using auto completion, REPL, etc.

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
  :config
  ; Disable flymake and use flycheck mode.
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  :diminish "")

;; Provide an customization option to set the virtualenvs WORKON_HOME directory.
(defcustom emacs-fast/workon-home nil
  "The default WORKON directory for the `pyvenv-workon' command.

The default WORKON directory for the `pyeven-workon' command is read from the
$WORKON_HOME shell variable.  When set to automatic and WORKON_HOME variable is
not inherited by Emacs, WORKON_HOME is set to the default Conda environemnt
folder, if any."
  :group 'emacs-fast
  :type '(choice
          (const :tag "Automatic" nil)
          (directory :tag "Manual")))

; If a location is specified, use it,
; else find and set to default Conda environment
; folder, if any.
(if emacs-fast/workon-home
    (setenv "WORKON_HOME" emacs-fast/workon-home)
  (unless (getenv "WORKON_HOME")
    (setenv "WORKON_HOME"
            (when (executable-find "conda")
              (require 'json)
              (let* ((json-object-type 'hash-table)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (data (json-read-from-string
                            (shell-command-to-string "conda info --json 2> /dev/null"))))
                (car (gethash "envs_dirs" data)))))))

(provide 'femacs-python)
;;; femacs-python.el ends here
