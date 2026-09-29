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

;; Each recipe is (LANG URL TAG [SOURCE-DIR]).  The tags build the same parsers
;; as the commits Emacs 31 pins in its own recipes (all tree-sitter ABI 14).
;; Tags, rather than commits, keep the recipes usable on Emacs 30, whose
;; installer only understands a branch or tag in the REVISION slot.
(defconst femacs/treesit-language-sources
  '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
    (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.5")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.2")
    (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.1"))
  "Pinned native Tree-sitter grammar sources used by femacs.")

(defconst femacs/treesit-mode-remaps
  '((python (python-mode) python-ts-mode)
    (c (c-mode) c-ts-mode)
    (cpp (c++-mode) c++-ts-mode)
    (javascript (js-mode javascript-mode) js-ts-mode)
    (css (css-mode) css-ts-mode))
  "Entries (LANG FROM-MODES TS-MODE): use TS-MODE once LANG's grammar exists.")

(defun femacs/treesit-enable-modes ()
  "Switch to native Tree-sitter modes for every installed grammar."
  (dolist (remap femacs/treesit-mode-remaps)
    (when (treesit-ready-p (car remap) t)
      (dolist (from (nth 1 remap))
        (add-to-list 'major-mode-remap-alist (cons from (nth 2 remap))))))
  ;; Emacs 31 maps these files to `typescript-ts-mode-maybe' and
  ;; `tsx-ts-mode-maybe'.  Emacs 30 only adds its entries when
  ;; typescript-ts-mode.el is loaded, so a fresh session opens them in
  ;; `fundamental-mode'.  Register them ourselves there.
  (unless (fboundp 'tsx-ts-mode-maybe)
    (when (treesit-ready-p 'typescript t)
      (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
    (when (treesit-ready-p 'tsx t)
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))))

(when (treesit-available-p)
  (dolist (source femacs/treesit-language-sources)
    (setf (alist-get (car source) treesit-language-source-alist) (cdr source)))
  (femacs/treesit-enable-modes))

(defun femacs/treesit-install-grammars ()
  "Install any missing native Tree-sitter grammars used by femacs.
Buffers opened afterwards use the native modes; reopen existing ones."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs was built without native Tree-sitter support"))
  (dolist (source femacs/treesit-language-sources)
    (unless (treesit-ready-p (car source) t)
      (treesit-install-language-grammar (car source))))
  (femacs/treesit-enable-modes))

(provide 'femacs-treesitter)

;;; femacs-treesitter.el ends here
