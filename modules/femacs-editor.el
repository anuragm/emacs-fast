;;; femacs-editor.el --- Editor enhancement in fast Emacs.
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Editor specific enhancements like line number mode, rainbow delimiters, etc.

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

;; Use rainbow delimiter mode
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

;; Use nlinum mode, a faster alternative to linum mode
(use-package nlinum
  :ensure t
  :commands (nlinum-mode))

;;Whitespace mode.
(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :init
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (diminish 'whitespace-mode "ⓦ"))

;; Show matching braces and parenthesis.
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Show diffs in fringe
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode-unless-remote)
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Use CUA mode for rectangular sections.
(setq cua-enable-cua-keys nil)
(cua-mode 1)

;; When in prettify-symbol-mode, expand when point is at the symbol.
(setq prettify-symbols-unprettify-at-point 'right-edge)

(provide 'femacs-editor)
;;; femacs-editor.el ends here
