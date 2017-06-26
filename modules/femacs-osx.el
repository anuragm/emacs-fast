;;; femacs-osx.el --- OS X Specific configuration
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configures OS X.

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

;; Use CMD as Meta, Alt as Super, Fn as Hyper
(setq mac-command-modifier  'meta)
(setq mac-option-modifier   'super)
(setq mac-function-modifier 'hyper)
(setq mac-control-modifier  'control)

;; Helper function to check if we are using EMacs mac port.
(defun emacs-mac-p ()
  "Return t if we are running emacs-mac port, return nil otherwise."
  (featurep 'mac))

;; Read path variable from command line, if not manually customized.
(defcustom emacs-fast/exec-path nil
  "Custom exec path for Emacs.

When not called from shell, Emacs does not inherits $PATH from shell.  When nil,
paths are read by launching a login shell, which might be slow with complicated
configuration.  Manually specifying the path will make the process faster."
  :group 'emacs-fast
  :type '(repeat directory))

(if emacs-fast/exec-path
    (progn
      (setq exec-path emacs-fast/exec-path)
      (setenv "PATH" (mapconcat 'identity exec-path ":")))
  (use-package exec-path-from-shell
    :ensure t
    :demand
    :config
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

;; Don't delete directly, use Trash
(setq delete-by-moving-to-trash t)

;; Use system browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Some helper functions.
(use-package dired-x
  :ensure nil)

(defun finder-here ()
  "Opens current buffer's directory in Finder."
  (interactive)
  (dired-smart-shell-command "open \"$PWD\"" nil nil))

(defun iterm-here ()
  "Opens current buffer's directory in iTerm."
  (interactive)
  (dired-smart-shell-command "open -a iTerm2 \"$PWD\"" nil nil))

;; Use gls for dired mode if installed.
(when (executable-find "gls")
  (setq insert-directory-program (executable-find "gls")))

;; Use Skim.app to view PDFs when available.
(with-eval-after-load "tex"
  (when (file-exists-p "/Applications/Skim.app")
    ;; Add Skim as a PDF viewer to AucTeX list.
    (push
     '("Skim"
       "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")
     TeX-view-program-list)
    (push '(output-pdf "Skim") TeX-view-program-selection)))

(provide 'femacs-osx)
;;; femacs-osx.el ends here
