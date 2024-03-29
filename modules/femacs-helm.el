;;; femacs-helm.el --- Helm configuration
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configures helm to be applied almost everywhere, along with flx search.

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
(use-package helm
  :ensure t
  :diminish helm-mode
  :defines helm-net-prefer-curl
  :bind (("C-x b"     . helm-mini)
         ("C-x C-b"   . helm-buffers-list)
         ("C-x C-f"   . helm-find-files)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         ("C-c f"     . helm-recentf)
         ("C-c h"     . helm-command-prefix)
         :map helm-map
         ("<left>"  . helm-previous-source)
         ("<right>" . helm-next-source))
  :custom
  (helm-ff-lynx-style-map t)       ; Use left/right for navigation in helm-files
  (helm-imenu-lynx-style-map t)    ; Use left/right for navigation in helm-imenu
  (helm-semantic-lynx-style-map t) ; Use left/right for navigation in helm-semantic
  (helm-split-window-inside-p t)   ; Split in current window
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-candidate-number-limit 500)      ; Limit candidates.
  (helm-ff-file-name-history-use-recentf t) ; Use standard file history.
  (helm-M-x-fuzzy-match nil)       ; Turn off fuzzy match for M-x
  :init
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (global-unset-key (kbd "C-x c")) ; Remove the default key prefix.
  :config
  (progn
    (load "helm-autoloads" nil t)
    (helm-mode 1)))

(provide 'femacs-helm)
;;; femacs-helm.el ends here
