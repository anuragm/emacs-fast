;; -*- lexical-binding: t; -*-
;;; femacs-web.el --- TypeScript, JavaScript, React and CSS editing
;;
;; Copyright © 2016-2026 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Uses the native Tree-sitter modes (`typescript-ts-mode', `tsx-ts-mode',
;; `js-ts-mode', `css-ts-mode') with lsp-mode.  In TS/JS buffers lsp-mode starts
;; typescript-language-server and, as an add-on, the ESLint server, which picks
;; up the project's own eslint from node_modules.  Prettier formatting on save
;; is enabled only in projects that carry a Prettier config.
;;
;; One-time setup: M-x femacs/treesit-install-grammars, then
;; M-x lsp-install-server for ts-ls and eslint.

;;; License:

;; Copyright (c) 2016-2026 Anurag Mishra

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

(defvar typescript-ts-mode-indent-offset)
(defvar js-indent-level)
(defvar css-indent-offset)
(defvar lsp-eslint-auto-fix-on-save)

(setq typescript-ts-mode-indent-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Tell ESLint server to apply its autofixes on save, like `npm run format'.
(with-eval-after-load 'lsp-eslint
  (setq lsp-eslint-auto-fix-on-save t))

;; Runs the project's own prettier (via node_modules) asynchronously on save.
(use-package apheleia
  :commands (apheleia-mode apheleia-format-buffer)
  :config
  (diminish 'apheleia-mode))

(defconst femacs/prettier-config-files
  '(".prettierrc" ".prettierrc.json" ".prettierrc.yaml" ".prettierrc.yml"
    ".prettierrc.js" ".prettierrc.cjs" ".prettierrc.mjs" ".prettierrc.toml"
    "prettier.config.js" "prettier.config.cjs" "prettier.config.mjs")
  "Files whose presence means a project formats its code with Prettier.")

(defun femacs/prettier-project-p ()
  "Return non-nil if the current buffer's file sits under a Prettier config."
  (when buffer-file-name
    (locate-dominating-file
     buffer-file-name
     (lambda (dir)
       (seq-some (lambda (f) (file-exists-p (expand-file-name f dir)))
                 femacs/prettier-config-files)))))

(defun femacs/web-mode-setup ()
  "Configure TypeScript, JavaScript and CSS editing buffers."
  ;; JSON modes derive from `js-mode'; they are configured in femacs-json.
  (unless (derived-mode-p 'js-json-mode 'json-mode 'json-ts-mode)
    (setq-local fill-column 100)        ; Prettier's usual printWidth.
    (display-line-numbers-mode)
    (company-mode)
    (whitespace-mode)
    (when (femacs/prettier-project-p)
      (apheleia-mode))
    (lsp)))

(add-hook 'typescript-ts-base-mode-hook #'femacs/web-mode-setup)
(add-hook 'js-base-mode-hook #'femacs/web-mode-setup)
(add-hook 'css-ts-mode-hook #'femacs/web-mode-setup)

(provide 'femacs-web)
;;; femacs-web.el ends here
