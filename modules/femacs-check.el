;;; femacs-check.el --- Adds checks to different buffers
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Spell check for text mode and Flycheck for programming modes.

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

;; Add hunspell/aspell as dictionary back-ends.
(use-package ispell
  :ensure nil
  :defer t
  :init
  (defvar ispell-program-name)
  (defvar ispell-extra-args)
  (if (executable-find "hunspell")
      (progn
        (setq ispell-program-name "hunspell")
        (setq ispell-local-dictionary "en_US")
        (setq ispell-local-dictionary-alist
              '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']"
                 nil ("-d" "en_US") nil utf-8))))
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))))

(use-package flyspell
  :ensure nil
  :commands (flyspell-correct-word)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (diminish 'flyspell-mode "ⓕ")
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  (define-key flyspell-mouse-map [down-mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-2] nil)
  (diminish 'flyspell-mode "ⓕ"))

;; Describe word looks up an English dictionary online.
(use-package define-word
  :ensure t
  :commands (define-word-at-point)
  :bind
  (("C-c d". define-word-at-point)
   ("C-c D" . define-word)))

;; Initialize flycheck mode for modes that need it.
(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)   ; Let flycheck.el see loaded paths.
  :defer t
  :config
  (require 'flycheck-ruff)
  (diminish 'flycheck-mode "Ⓕ"))

(provide 'femacs-check)
;;; femacs-check.el ends here
