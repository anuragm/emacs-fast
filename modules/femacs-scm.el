;;; femacs-scm.el --- Source code management for Emacs-fast
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds Magit mode for managing git and other configurations for source code management.

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

;; Install Magit with sensible defaults.
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind ("C-x g" . magit-status)
  :custom
  (magit-delete-by-moving-to-trash t)
  (magit-diff-refine-hunk nil)
  (magit-diff-highlight-hunk-body nil)
  (magit-section-visibility-indicator nil)
  :init
  (add-hook 'git-commit-setup-hook 'whitespace-mode)
  :config
  (magit-auto-revert-mode)
  (diminish 'auto-revert-mode))

;; Manage identities using git-identities
(use-package git-identity
  :ensure t
  :after magit
  :custom
  (git-identity-verify t)  ;Warn if the global identity setting violates policy
  (git-identity-default-username "Anurag Mishra")
  :config
  ;; Set git-identity-list with my emails
  (require 'git-identity-magit)
  (setq git-identity-list
        '(("anurag@mishra.xyz")
          ("mishra.anurag07@gmail.com")
          ("anurag.mishra@q-ctrl.com")))
  (git-identity-magit-mode 1)
  (define-key magit-status-mode-map (kbd "I") 'git-identity-info))

;; And git modes
(use-package gitconfig-mode
  :ensure t)
(use-package gitignore-mode
  :ensure t)
(use-package gitattributes-mode
  :ensure t)

;; Add git time machine with its Hydra menu
(use-package git-timemachine
  :ensure t
  :after hydra
  :commands (git-timemachine)
  :bind (:map git-timemachine-mode-map ("`" . hydra-git-timemachine/body))
  :init
  (add-hook 'git-timemachine-mode-hook #'hydra-git-timemachine/body)
  :hydra (hydra-git-timemachine (:hint nil)
  "
               Git time-machine
_p_: previous  _n_: next _b_: blame _c_: show commit
"
  ("p" git-timemachine-show-previous-revision)
  ("n" git-timemachine-show-next-revision)
  ("b" git-timemachine-blame)
  ("c" git-timemachine-show-commit)
  ("." nil "cancel" :color blue)
  ("q" git-timemachine-quit "quit" :color blue)))

;; Add helm package for listing git files
(use-package helm-ls-git
  :ensure t
  :commands (helm-ls-git-ls)
  :bind ("C-x C-d" . helm-browse-project))

;; Show diffs in fringe
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode-unless-remote diff-hl-magit-post-refresh)
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'femacs-scm)
;;; femacs-scm.el ends here
