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

(require 'treesit)

(defconst femacs/treesit-language-sources
  '((python "https://github.com/tree-sitter/tree-sitter-python" "26855eabccb19c6abf499fbc5b8dc7cc9ab8bc64")
    (c "https://github.com/tree-sitter/tree-sitter-c" "b780e47fc780ddc8da13afa35a3f4ed5c157823d")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "8b5b49eb196bec7040441bee33b2c9a4838d6967"))
  "Pinned native Tree-sitter grammar sources used by femacs.")

(when (treesit-available-p)
  (dolist (source femacs/treesit-language-sources)
    (setf (alist-get (car source) treesit-language-source-alist) (cdr source)))
  (dolist (remap '((python python-mode python-ts-mode)
                   (c c-mode c-ts-mode)
                   (cpp c++-mode c++-ts-mode)))
    (when (treesit-ready-p (car remap) t)
      (add-to-list 'major-mode-remap-alist
                   (cons (nth 1 remap) (nth 2 remap))))))

(defun femacs/treesit-install-grammars ()
  "Install any missing native Tree-sitter grammars used by femacs."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs was built without native Tree-sitter support"))
  (dolist (source femacs/treesit-language-sources)
    (unless (treesit-ready-p (car source) t)
      (treesit-install-language-grammar (car source)))))

(provide 'femacs-treesitter)

;;; femacs-treesitter.el ends here
