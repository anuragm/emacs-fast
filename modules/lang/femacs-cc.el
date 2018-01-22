;;; femacs-cc.el --- Binds for CC modes
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: programming, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains various configurations for C++/C mode, especially using Irony mode as
;; completion engine.

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

;; Update CC mode if required. Emacs might bundle old CC modes.
;; Always pulls the latest alpha version. Specifying branch for hg is not support by
;; melpa.
(use-package cc-mode
  :quelpa
  (cc-mode :fetcher hg
           :url "http://hg.code.sf.net/p/cc-mode/cc-mode"))

;; Use Irony mode for auto-completion of code and header files.
(defvar custom-irony-directory
  (file-name-as-directory (expand-file-name "private/irony" femacs-dir))
  "Custom directory location for Irony files.")

(use-package irony
  :ensure t
  :commands (irony-mode company-irony irony-mode-hook)
  :init
  ;;Configure compilation options for irony mode
  (setq irony-server-install-prefix custom-irony-directory)
  (setq irony-user-dir custom-irony-directory)
  (add-hook 'irony-mode-hook
            '(lambda ()
               (define-key irony-mode-map [remap completion-at-point]
                 'irony-completion-at-point-async)
               (define-key irony-mode-map [remap complete-symbol]
                 'irony-completion-at-point-async)))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (diminish 'irony-mode "Ⓘ"))

;; Use company-irony as company mode back-end.
(use-package company-irony
  :ensure t
  :commands (company-irony))

;; Use company-c-headers to complete header files.
(use-package company-c-headers
  :ensure t
  :commands (company-c-headers)
  :config
  (setq company-c-headers-path-user '("." "./include")))

;; Set ggtags mode
(use-package ggtags
  :ensure t
  :commands (ggtags-mode)
  :init
  (setq ggtags-completing-read-function nil)
  :config
  (diminish 'ggtags-mode "Ⓖ"))

;; Use flycheck-irony in CC mode.
(use-package flycheck-irony
  :ensure t
  :commands (flycheck-irony-setup)
  :after (flycheck)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Set irony-eldoc for function signatures in mini-buffer.
(use-package irony-eldoc
  :quelpa (irony-eldoc :fetcher github :repo "josteink/irony-eldoc")
  :commands (irony-eldoc)
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; Use dtrt-indent to auto-detect indentation style in a file.
(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-mode)
  :init
  (setq dtrt-indent-active-mode-line-info nil)
  :config
  (diminish 'dtrt-indent-mode))

;; Keep company-dabbrev at end since it can always complete some stupid thing.
(defvar cc-mode-backends
  '(company-files
    company-c-headers
    company-irony
    company-gtags
    company-dabbrev-code)
  "Company back-ends to be used in CC mode.")

;; Set variables first before loading modes.
(add-hook
 'c-mode-common-hook
 '(lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode)
      (run-hooks 'prog-mode-hook) ; Run prog-mode hook since cc-mode does not derives from it.
      (setq-local company-backends cc-mode-backends)
      (setq-local fill-column 90)
      (setq-local whitespace-line-column 90)
      (auto-fill-mode)
      (flycheck-mode 1)
      (yas-minor-mode 1)
      (irony-mode 1)
      (ggtags-mode 1)
      (whitespace-mode)
      (company-mode)
      (eldoc-mode)
      (dtrt-indent-mode))))

(provide 'femacs-cc)
;;; femacs-cc.el ends here
