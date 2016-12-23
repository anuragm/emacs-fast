;;; femacs-matlab.el --- MATLAB mode configuration
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains configurations for MATLAB mode

;;; License:

;; Copyright (c) 2016 Anurag Mishra

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

(defun matlab-mode-features()
  (setq-local fill-column 95)
  (setq-local whitespace-line-column 95)
  (setq-local company-backends '(company-files (company-matlab company-dabbrev-code)))
  (nlinum-mode t)
  (company-mode t)
  (yas-minor-mode)
  (flycheck-mode)
  (whitespace-mode))

(use-package matlab-load
  :ensure matlab-mode
  :commands (matlab-mode matlab-shell company-matlab matlab-mode-common-setup)
  :mode ("\\.m\\'" . matlab-mode)
  :init
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
  (setq matlab-server-executable "/Applications/MATLAB_R2015b.app/bin/matlab")
  (setq matlab-server-buffer "*matlab-server*")
  (add-hook 'matlab-mode-hook 'matlab-mode-features)
  :config
  (with-eval-after-load 'company-dabbrev-code
    (push 'matlab-mode company-dabbrev-code-modes)))

(provide 'femacs-matlab)
;;; femacs-matlab.el ends here
