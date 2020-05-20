;;; femacs-matlab.el --- MATLAB mode configuration
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains configurations for MATLAB mode

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

(defvar company-backends)
(defvar femacs-lang-dir)
(defvar company-dabbrev-code-modes)

(defun matlab-mode-features()
  "Custom features for MATLAB mode."
  ;Run prog-mode hook since MATLAB mode does not derives from it.
  (run-hooks 'prog-mode-hook) ;
  (setq-local fill-column 95)
  (setq-local company-backends '(company-files (company-dabbrev-code company-gtags)))
  (matlab-functions-have-end-minor-mode)
  (company-mode t)
  (yas-minor-mode)
  (mlint-minor-mode)
  (ggtags-mode)
  (whitespace-mode))

(defun femacs/matlab-shell-features()
  "Custom features for MATLAB shell mode."
  (company-mode t)
  (yas-minor-mode))

(use-package matlab
  :ensure matlab-mode
  :commands
  (matlab-mode matlab-shell matlab-mode-common-setup mlint-minor-mode)
  :functions (matlab-functions-have-end-minor-mode)
  :mode ("\\.m\\'" . matlab-mode)
  :init
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
  (customize-set-variable
   'matlab-shell-command
   (expand-file-name "matlab_emacs_wrapper" femacs-lang-dir))
  :config
  (add-hook 'mlint-minor-mode-hook
            (lambda () (diminish 'mlint-minor-mode)))
  (with-eval-after-load 'company-dabbrev-code
    (push 'matlab-mode company-dabbrev-code-modes))
  (add-hook 'matlab-mode-hook 'matlab-mode-features)
  (add-hook 'matlab-shell-mode-hook 'femacs/matlab-shell-features))

(provide 'femacs-matlab)
;;; femacs-matlab.el ends here
