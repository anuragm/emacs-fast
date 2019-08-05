;;; femacs-editor.el --- Editor enhancement in fast Emacs.
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Editor specific enhancements like line number mode, rainbow delimiters, etc.

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


;; Line numbers

;; Add additional space to (n)Linum mode in terminal.
(defun linum-format-func (line)
  "Add a space to the `linum-mode' LINE."
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(unless window-system
  (defvar linum-format)
  (setq linum-format #'linum-format-func)
  (defvar nlinum-format)
  (setq nlinum-format "%d "))

;; For newer emacs, use the built-in display line number mode.
;; Else use nlinum mode, a faster alternative to linum mode
(when (version< emacs-version "26.1")
  (use-package nlinum
    :ensure t
    :commands (nlinum-mode)
    :init
    (add-hook 'prog-mode-hook 'nlinum-mode)))

(use-package display-line-numbers
  :ensure nil
  :if (not (version< emacs-version "26.1"))
  :init
  (defalias 'nlinum-mode 'display-line-numbers-mode)
  (defalias 'global-nlinum-mode 'global-display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; Formatting modes

;; Use rainbow delimiter mode
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;Whitespace mode.
(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :init
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (diminish 'whitespace-mode "ⓦ"))

;; Use dtrt-indent to auto-detect indentation style in a file for all programming
;; languages.
(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-mode)
  :custom (dtrt-indent-active-mode-line-info nil)
  :init
  (add-hook 'prog-mode-hook 'dtrt-indent-mode)
  :config
  (diminish 'dtrt-indent-mode))

;; Show matching braces and parenthesis.
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Use CUA mode for rectangular sections.
(use-package cua-base
  :ensure nil
  :custom (cua-enable-cua-keys nil)
  :init
  (cua-mode 1))

;; When in prettify-symbol-mode, expand when point is at the symbol.
(setq prettify-symbols-unprettify-at-point 'right-edge)


;; Navigation

;; Add Ace-jump mode for quick navigation.
(use-package ace-jump-mode
  :ensure t
  :commands (ace-jump-mode)
  :bind (("C-c SPC" . ace-jump-mode)))

;; Use Move buffer to swap windows.
(use-package buffer-move
  :ensure t
  :commands (buf-move buf-move-up buf-move-down buf-move-right buf-move-left)
  :bind (("<C-s-up>" . buf-move-up)
         ("<C-s-down>" . buf-move-down)
         ("<C-s-left>" . buf-move-left)
         ("<C-s-right>" . buf-move-right)))


;; Mode line

;; Add Word count minor mode to text modes
(use-package wc-mode
  :ensure t
  :init
  (add-hook 'find-file-hook
   (lambda ()
     (when (string= (file-name-extension buffer-file-name) "txt")
       (wc-mode)))))

;; Integrate ANZU, which gives number of total matches.
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :defer 1
  :init
  (setq anzu-cons-mode-line-p nil) ; Let Doomline show Anzu count.
  :commands (anzu-query-replace anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


;; EDiff customization
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Don't use new frame for Ediff
  (ediff-split-window-function 'split-window-vertically))  ; Split vertically always

(provide 'femacs-editor)
;;; femacs-editor.el ends here
