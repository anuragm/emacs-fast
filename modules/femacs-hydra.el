;;; femacs-hydra.el --- Hydra for defining simple menus
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file loads and install Hydra, and also contains definitions for Hydra that don't
;; logically fit in other packages.

;;; License:

;; Copyright (c) 2016-2021 Anurag Mishra

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

(use-package hydra
  :ensure t
  :init
  (setq lv-use-separator t))

(use-package use-package-hydra
  :ensure t)

;; Include all the code from Hydra sub-folder
(when (and (boundp 'femacs-module-path) femacs-module-path)
  (defvar femacs-hydra-dir (expand-file-name "hydra" femacs-module-path))
  (push femacs-hydra-dir load-path)
  (mapc (lambda (name)
          (require (intern (file-name-sans-extension name))))
        (directory-files femacs-hydra-dir nil "\\.el$")))

(provide 'femacs-hydra)
;;; femacs-hydra.el ends here
