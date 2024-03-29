;;; femacs-cc.el --- Binds for CC modes
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: programming, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains various configurations for C++/C mode, especially using Irony mode as
;; completion engine.

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

;; Update CC mode if required. Emacs might bundle old CC modes.
;; Always pulls the latest alpha version. Specifying branch for hg is not support by
;; melpa. Only do so if mercurial has been installed on the system.

(when (executable-find "hg")
  (use-package cc-mode
    :quelpa
    (cc-mode :fetcher hg
             :url "http://hg.code.sf.net/p/cc-mode/cc-mode")))

;; Use company-c-headers to complete header files.
(use-package company-c-headers
  :ensure t
  :commands (company-c-headers)
  :config
  (setq company-c-headers-path-user '("." "./include")))

;; Set variables first before loading modes.
(add-hook
 'c-mode-common-hook
 (lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode)
      (run-hooks 'prog-mode-hook) ; Run prog-mode hook since cc-mode does not derives from it.
      (setq-local fill-column 90)
      (auto-fill-mode)
      (flycheck-mode 1)
      (yas-minor-mode 1)
      (whitespace-mode)
      (company-mode)
      (tree-sitter-hl-mode)
      (dtrt-indent-mode)
      (lsp))))

(provide 'femacs-cc)
;;; femacs-cc.el ends here
