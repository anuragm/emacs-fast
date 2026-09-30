;; -*- lexical-binding: nil; -*-
;;; femacs-lsp.el --- Language server integration

;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;;; Commentary:

;; Based on LSP package.  May switch to elgot when it is released.

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

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  ;; Git worktrees live in <repo>/worktrees/.  They are separate projects with
  ;; their own LSP sessions, so the main checkout should not watch them.
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]worktrees\\'")
  (advice-add 'lsp-find-session-folder :around
              #'femacs/lsp-session-folder-within-project))

(declare-function lsp-f-ancestor-of? "lsp-mode")

(defun femacs/lsp-session-folder-within-project (orig session file-name)
  "Call ORIG, but reject session folders above FILE-NAME's project root.
lsp-mode assigns a file to the deepest known session folder containing it,
so once <repo> is imported, files in <repo>/worktrees/<name> would join the
main checkout's servers.  A git worktree is its own `project.el' project;
refusing a folder above that root makes lsp-mode offer to import the worktree
as a new root with its own servers."
  (let ((folder (funcall orig session file-name)))
    (if-let* ((folder)
              (project (project-current nil (file-name-directory file-name)))
              (root (project-root project))
              ((lsp-f-ancestor-of? folder root)))
        nil
      folder)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package treemacs
  :commands treemacs)

;; all-the-icons is used only for the Treemacs theme.
(use-package all-the-icons
  :after treemacs
  :custom
  (all-the-icons-scale-factor 1.1)
  :config
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (when (display-graphic-p)
    (treemacs-load-theme "all-the-icons")))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols))

(provide 'femacs-lsp)
;;; femacs-lsp.el ends here
