;;; femacs-cmake.el --- CMake additions
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds support for CMake and related utilities to Emacs.

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

(use-package cmake-mode
  :ensure t
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Use CMake IDE to make C++ programming setup easier.
(use-package cmake-ide
  :ensure t
  :init (cmake-ide-setup)
  :commands (cmake-ide-compile cmake-ide-run-cmake))

(add-hook
 'cmake-mode-hook
 '(lambda()
    (nlinum-mode)
    (company-mode)
    (yas-minor-mode)
    (whitespace-mode)
    (auto-fill-mode)))

(provide 'femacs-cmake)
;;; femacs-cmake.el ends here
