;;; flycheck-ruff.el --- Flycheck checker using Python's ruff -*- lexical-binding: t -*-

;; Author: Anurag Mishra
;; Maintainer: Anurag Mishra
;; Version: 1.0.0
;; Package-Requires: (python flycheck)
;; Homepage: homepage
;; Keywords: checker

;;; Commentary:

;; A Python syntax and style checker using the ruff utility.
;; Code take from here,
;; https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
;; which itself is adapted from here, https://github.com/Wilfred/flycheck-pyflakes.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'flycheck)

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
  To override the path to the ruff executable, set
  `flycheck-python-ruff-executable'.
  See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-mode
  :next-checkers (python-pycompile python-mypy))

(add-to-list 'flycheck-checkers 'python-ruff)

(provide 'flycheck-ruff)
;;; flycheck-ruff.el ends here
