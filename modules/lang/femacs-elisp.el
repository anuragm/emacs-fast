;;; femacs-elisp.el --- Support for editing Emacs lisp files.
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds useful modes and enables fly-check

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

;; CAUTION : Set variables first before loading modes.
(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (unless (string= "*scratch*" (buffer-name))
      (setq-local fill-column 90)
      (setq-local whitespace-line-column 90)
      (auto-fill-mode)
      (flycheck-mode 1)
      (yas-minor-mode 1)
      (whitespace-mode)
      (company-mode)
      (eldoc-mode))))

(provide 'femacs-elisp)
;;; femacs-elisp.el ends here
