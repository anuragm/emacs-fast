;;; femacs-ui.el --- Adds UI elements.
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds various UI elements, including theme.

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

;; Use diminish package to clean up the bar
(use-package diminish
  :ensure t)

;; Remove various cruft from UI.
(tool-bar-mode      -1)
(scroll-bar-mode    -1)
(line-number-mode    1)
(column-number-mode  1)
(blink-cursor-mode  -1)
(setq read-file-name-completion-ignore-case t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;Use shift-<left/right/up/down> to change windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Give a visual representation to switch windows
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

;; Remove the annoying bell sound.
(setq visible-bell 1)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit ))
          (ding))))

;; Add material theme.
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'wave)
  :config
  (spaceline-emacs-theme))

;; Show key config for shortcuts.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  :config (which-key-mode))

;; Add Beacon mode for highlighted cursor
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

;; Diminish other modes
(diminish 'auto-fill-mode "ⓐ")

(provide 'femacs-ui)
;;; femacs-ui.el ends here
