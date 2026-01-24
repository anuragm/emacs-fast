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

;; Cached WORKON_HOME directory for fast startup.
(defcustom femacs/workon-home nil
  "Cached WORKON directory for the `pyvenv-workon' command.

This variable is automatically populated from conda and kept in sync.
Set manually to override automatic detection."
  :group 'femacs
  :type '(choice
          (const :tag "Automatic" nil)
          (directory :tag "Manual")))

(defun femacs/get-conda-envs-dir ()
  "Get conda envs directory by running conda info --json."
  (when (executable-find "conda")
    (require 'json)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (data (json-read-from-string
                  (shell-command-to-string "conda info --json 2> /dev/null"))))
      (car (gethash "envs_dirs" data)))))

(defun femacs/sync-workon-home ()
  "Sync WORKON_HOME from conda if it differs from cached."
  (let ((conda-path (femacs/get-conda-envs-dir)))
    (when (and conda-path (not (equal femacs/workon-home conda-path)))
      (customize-save-variable 'femacs/workon-home conda-path)
      (setenv "WORKON_HOME" conda-path)
      (message "Synced WORKON_HOME from conda"))))

;; Use cached value at startup, sync in background
(if femacs/workon-home
    (progn
      (setenv "WORKON_HOME" femacs/workon-home)
      (run-with-idle-timer 60 nil #'femacs/sync-workon-home))
  (unless (getenv "WORKON_HOME")
    (let ((conda-path (femacs/get-conda-envs-dir)))
      (when conda-path
        (setenv "WORKON_HOME" conda-path)
        (customize-save-variable 'femacs/workon-home conda-path)))))


;; Use LSP Pyright for IDE features.
(use-package lsp-pyright
  :defer t
  ;; this allows for separate LSP servers for seperate projects.
  :init (setq lsp-pyright-multi-root nil)
  :config
  (setq lsp-pyright-venv-path (getenv "WORKON_HOME")))

;; Shows indentation lines for code.
(use-package highlight-indentation
  :commands highlight-indentation-mode)

;; isort mode automatically sorts headers.
(use-package python-isort
  :after python)

;; black to reformat python code
(use-package python-black
  :after python)

;; Pyvenv mode to change virtual environments.
(use-package pyvenv
  :commands (pyvenv-workon pyvenv-activate pyvenv-tracking-mode))

;; Ruff to lint/format code
(use-package ruff-format
  :after python)

;; Setup the python mode.
;; Format/lint tool should be enabled on folder by folder basis.
(defun femacs/python-mode-hook()
  "Custom bindings for python mode."
  (setq-local fill-column 90)
  (electric-indent-mode)
  (display-line-numbers-mode)
  (company-mode)
  (whitespace-mode)
  (dtrt-indent-mode)
  (highlight-indentation-mode)
  (tree-sitter-hl-mode)
  (pyvenv-tracking-mode)
  (require 'lsp-pyright)
  (lsp))

(add-hook 'python-mode-hook #'femacs/python-mode-hook)

(provide 'femacs-python)
;;; femacs-python.el ends here
