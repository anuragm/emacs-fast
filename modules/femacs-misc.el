;;; femacs-misc.el --- Contains miscellaneous packages.
;;
;; Copyright © 2016 Anurag Mishra
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

;; Copyright (c) 2016 Anurag Mishra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  :commands (persistent-scratch-setup-default persistent-scratch-autosave-mode)
  :init
  (setq persistent-scratch-backup-directory femacs/persistent-scratch-dir)
  (setq persistent-scratch-save-file
        (expand-file-name "last-scratch" femacs/persistent-scratch-dir))
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode))

(provide 'femacs-misc)
;;; femacs-misc.el ends here
