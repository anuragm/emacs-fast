;;; femacs-core.el --- Contains core fast EMacs functionality
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Defines function which are used later for setting up packages.

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

;; Add a way to multiple align
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Useful misc keybindings
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Save a recent list of files, always
(use-package recentf
  :ensure nil
  :demand
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 500
        recentf-auto-cleanup 'never)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (recentf-mode 1))

;; Keep recent list of commands persistent across sessions
(use-package savehist
  :ensure nil
  :demand
  :init
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-autosave-interval 60)
  :config
  (savehist-mode 1))

;; Let flycheck.el see loaded paths.
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Don't warn about the commands
(put 'erase-buffer 'disabled nil)

;; Backup options
(setq make-backup-files    t ;Enable file backup
      vc-make-backup-files t ; and also backup version controlled files
      version-control      t ; Store multiple named backups
      kept-new-versions   10 ; Store 10 new backups
      kept-old-versions    3 ; And 3 of the very original ones
      delete-old-versions  t ; Delete without asking
      backup-by-copying-when-linked t ; Don't clobber hard links
      backup-by-copying-when-privileged-mismatch t ; Nor root files
      backup-directory-alist ; Location of backup
      `(("." . ,(concat user-emacs-directory "private/backups"))))

;; Auto file save options
(setq auto-save-file-name-transforms ; Location of auto save files.
      `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix     ; Location of session recovery file
      (concat user-emacs-directory "private/auto-save-list"))

(provide 'femacs-core)
;;; femacs-core.el ends here
