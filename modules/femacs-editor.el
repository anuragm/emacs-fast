;;; femacs-editor.el --- Editor enhancement in fast Emacs.
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Editor specific enhancements like line number mode, rainbow delimiters, etc.

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


;; Line numbers

(use-package display-line-numbers
  :ensure nil
  :if (not (version< emacs-version "26.1"))
  :init
  (defalias 'nlinum-mode 'display-line-numbers-mode)
  (defalias 'global-nlinum-mode 'global-display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; Formatting modes and settings

;; Set default fill-column to 80 instead of 70.
(setq-default fill-column 80)

;; Use rainbow delimiter mode
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Whitespace mode.
(defvar emacs-fast--reenable-whitespace-mode nil)
(use-package whitespace
  :ensure nil
  :demand
  :init
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (setq whitespace-line-column nil) ; Let white space follow fill-column
  ; White space mode doesn't reset highlighting when variables are changed after loading
  ; the mode. We want to reload the mode every time local variable change
  ; after the mode is loaded, for eg, when a .dir-local.el file is used to specify
  ; directory local variables. See emacs.stackexchange.com/questions/7743
  (add-hook 'before-hack-local-variables-hook
            (lambda()
              (when whitespace-mode
                (whitespace-mode -1)
                (setq emacs-fast--reenable-whitespace-mode t))))
  (add-hook 'hack-local-variables-hook
            (lambda()
              (when emacs-fast--reenable-whitespace-mode
                (setq emacs-fast--reenable-whitespace-mode nil)
                (whitespace-mode +1))))
  :config
  (diminish 'whitespace-mode "ⓦ"))

;; Use separate whitespace cleanup mode. This mode cleans space iff the original files had
;; no trailing whitespace, etc. This is handy when collaborating with other people.
;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :ensure t
  :commands (whitespace-cleanup-mode global-whitespace-cleanup-mode)
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  :config
  (diminish 'whitespace-cleanup-mode))

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

;; Add Avy for quick navigation. https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :commands (avy-goto-char-2)
  :bind (("C-c SPC" . avy-goto-char-2)))

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


;; Code folding

;; hideshow-org mode intelligently folds code with <TAB> key. We won't globally enable
;; this mode, since not all languages might support it properly.
(use-package hideshow-org
  :ensure t
  :commands (hs-org/minor-mode))

;;; femacs-editor.el ends here
