;;; femacs-shell.el --- Shell scripting management (bash/zsh/etc)
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Add features for shell mode

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

(defun emacs-fast/sh-mode-hook ()
  "Settings for shell mode."
  (yas-minor-mode)
  (nlinum-mode)
  (auto-fill-mode)
  (flycheck-mode 1)
  (company-mode))

(use-package sh-mode
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.pbs\\'" . sh-mode))
  :init
  (add-hook 'sh-mode-hook #'emacs-fast/sh-mode-hook))

(provide 'femacs-shell)
;;; femacs-shell.el ends here
