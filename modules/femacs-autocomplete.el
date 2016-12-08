;;; femacs-autocomplete.el --- Adds auto-completion packages to EMacs
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; While EMacs provides for some in-build auto-completion, other
;; packages like Company, Yasnippets, etc provide much better
;; frameworks.  This file provides and configures these frameworks with
;; reasonable default, and then they can be used in each mode as
;; needed.

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

;; Company, an auto-completion framework awesome!
(defvar company-dabbrev-code-ignore-case)
(defvar company-dabbrev-ignore-case)
(defvar company-dabbrev-downcase)
(use-package company
  :ensure t
  :commands (company-mode company-complete)
  :defer t
  :init
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-code-ignore-case nil)
    (setq company-dabbrev-downcase nil))
  :config
  (progn
    (diminish 'company-mode "ⓒ")))

;; Helm company, using helm for auto-complete suggestions
(use-package helm-company
  :ensure t
  :bind ("C-:" . helm-company))

;; Show help for completion candidates in Popups
(use-package company-quickhelp
  :ensure t
  :diminish company-quickhelp-mode
  :commands company-quickhelp-mode
  :bind
  ("s-h" . company-quickhelp-manual-begin)
  :init
  (setq company-quickhelp-delay nil)
  :config
  (add-hook 'prog-mode-hook 'company-quickhelp-mode))

;; Add Yasnippet for expanding useful snippets.
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-global-mode)
  :init
  (setq yas-snippet-dirs nil)
  (push 'yas-installed-snippets-dir yas-snippet-dirs)
  (push (concat user-emacs-directory "private/snippets") yas-snippet-dirs)
  :config
  (yas-reload-all)
  (diminish 'yas-minor-mode "ⓨ"))

(provide 'femacs-autocomplete)

;;; femacs-autocomplete.el ends here
