;;; femacs-scm.el --- Source code management for Emacs-fast
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds Magit mode for managing git and other configurations for source code management.

;;; License:

;; Copyright (c) 2016 Anurag Mishra, MIT License.

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

;; Install Magit with sensible defaults.
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (progn
    (add-hook 'git-commit-setup-hook 'whitespace-mode)
    (setq magit-delete-by-moving-to-trash t)
    (setq magit-diff-refine-hunk t)
    (setq magit-section-visibility-indicator nil))
  :config
  (progn
    (magit-auto-revert-mode)
    (diminish 'auto-revert-mode)))

;; And git modes
(use-package gitconfig-mode
  :ensure t)
(use-package gitignore-mode
  :ensure t)
(use-package gitattributes-mode
  :ensure t)

;; Add git time machine
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

;; Add helm package for listing git files
(use-package helm-ls-git
  :ensure t
  :commands (helm-ls-git-ls)
  :bind ("C-x C-d" . helm-browse-project))

(provide 'femacs-scm)
;;; femacs-scm.el ends here
