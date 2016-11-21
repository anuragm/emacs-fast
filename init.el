;;; init.el --- Start up point.
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initializes use-package for use. Other packages are installed with
;; use-package macros.

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

;; Get the location of init.el. Rest of the paths flow from this.
(defvar femacs-init-file (or buffer-file-name load-file-name))
(message "file name is %s" femacs-init-file)
(defvar femacs-dir (file-name-directory femacs-init-file))
(message "director is %s" femacs-dir)


;; Set the user directory as this directory.
(setq user-init-file femacs-init-file)
(setq user-emacs-directory femacs-dir)

;; Initialize MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Store custom configuration in custom.el
(setq custom-file (expand-file-name "custom.el" femacs-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Other default options.
(setq gc-cons-threshold 50000000) ;Increase garbage collector threshold
(setq large-file-warning-threshold 10000000) ;Warn on large files, 1 MB
(setq load-prefer-newer t) ;Prefer new byte code

;; Now start loading rest of the system.
(defvar femacs-module-path (expand-file-name "modules" femacs-dir))
(push femacs-module-path load-path)

;; And import the required stuff.
(require 'femacs-ui)

(message "all set!")
;; init.el ends here.
