;;; femacs-misc.el --- Contains miscellaneous packages.
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains miscellaneous packages which cannot be categorized under one of the other
;; files.

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

(defvar femacs-dir)
(defvar femacs/persistent-scratch-dir
        (file-name-as-directory
         (expand-file-name "private/persistent-scratch" femacs-dir))
        "Location for saving persistent scratch files.")
(unless (file-directory-p femacs/persistent-scratch-dir)
  (make-directory femacs/persistent-scratch-dir))

(use-package persistent-scratch
  :ensure t
  :commands
  (persistent-scratch-restore)
  :init
  (setq persistent-scratch-autosave-interval 30)
  (setq persistent-scratch-save-file
        (expand-file-name "last-scratch" femacs/persistent-scratch-dir))
  :config
  (persistent-scratch-autosave-mode +1))

(provide 'femacs-misc)
;;; femacs-misc.el ends here
