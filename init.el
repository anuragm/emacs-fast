;;; init.el --- Start up point.
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initializes use-package for use.  Other packages are installed with
;; use-package macros.

;;; License:

;; Copyright (c) 2016-2021 Anurag Mishra, MIT License.

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

;;Start with a large GC, and then narrow it at the end
(setq gc-cons-threshold most-positive-fixnum)

;; Always prefer new version of code, compiled or not.
(setq load-prefer-newer t)

;; Get the location of init.el. Rest of the paths flow from this.
(defvar femacs-init-file (or buffer-file-name load-file-name))
(defvar femacs-dir (file-name-directory femacs-init-file))

;; Set the user directory as this directory.
(setq user-init-file femacs-init-file)
(setq user-emacs-directory femacs-dir)

;; Initialize packages.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
(setq package-archive-priorities '(("gnu"          . 10)
                                   ("built-in"     . 10)
                                   ("melpa-stable" . 10)
                                   ("melpa"        . 10)
                                   ("jcs-elpa"     .  5)))
(package-initialize)

;; Store custom configuration in custom.el
(setq custom-file (expand-file-name "private/custom.el" femacs-dir))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
(require 'use-package))
(require 'bind-key)
(use-package diminish
  :ensure t
  :commands (diminish))

;; Add benchmarking code
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Bootstrap QELPA.
(defvar quelpa-checkout-melpa-p)
(defvar quelpa-melpa-recipe-stores)
(defvar quelpa-self-upgrade-p)
(setq quelpa-checkout-melpa-p nil)
(setq quelpa-melpa-recipe-stores nil)
(setq quelpa-self-upgrade-p nil)
(use-package quelpa
  :ensure t
  :init
  (require 'quelpa))

;; Bootstrap quelpa-use-package
(use-package quelpa-use-package
  :ensure t
  :init
  (require 'quelpa-use-package))

;; Other default options.
(setq large-file-warning-threshold 10000000) ;Warn on large files, 1 MB
(setq load-prefer-newer t) ;Prefer new byte code
(setq confirm-kill-emacs 'yes-or-no-p) ; Ask before exiting Emacs

;; Now start loading rest of the system.
(defvar femacs-module-path (expand-file-name "modules" femacs-dir))
(push femacs-module-path load-path)

;; Import OS related stuff.first.
(when (eq system-type 'darwin)
  (require 'femacs-osx))

;; And import the required stuff.
(require 'femacs-hydra)
(require 'femacs-core)
(require 'femacs-ui)
(require 'femacs-editor)
(require 'femacs-check)
(require 'femacs-autocomplete)
(require 'femacs-helm)
(require 'femacs-scm)
(require 'femacs-helm-ag)
(require 'femacs-misc)
(require 'femacs-org)
(require 'femacs-lsp)
(require 'femacs-treesitter)

;;Additional language support.
(require 'femacs-lang)

;;Restore garbage size and collect it on idle.
(setq gc-cons-threshold (* 200 1024 1024)) ; 200 MB
(run-with-idle-timer 10 t 'garbage-collect)

;; Maximize the initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;Inhibit startup screen
(setq-default inhibit-startup-screen t)

(provide 'init)
;;; init.el ends here
