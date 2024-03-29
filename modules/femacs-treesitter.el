;;; femacs-treesitter.el --- Treesitter integration -*- lexical-binding: t -*-

;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;;; Commentary:

;; Integration for tree-sitter, a software module that allows for better semantic
;; understanding of the code.

;;; License:

;; Copyright (c) 2016-2021 Anurag Mishra, MIT License.

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

;; TODO: Switch to inbuilt version when released with Emacs 29.
;; We define hook here instead of individual languages. In future, it is expected that
;; programming modes will automatically use tree-sitter, at which point we can centrally
;; remove this command.
(use-package tree-sitter
  :ensure t
  :hook (python-mode . tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Use tree-sitter to automatically insert proper docstrings.
(use-package ts-docstr
  :ensure t
  :commands (ts-docstr-at-point ts-docstr-mode))

(provide 'femacs-treesitter)

;;; femacs-treesitter.el ends here
