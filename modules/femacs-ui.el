;;; femacs-ui.el --- Adds UI elements.
;;
;; Copyright © 2016-2018 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds various UI elements, including theme.

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


;; Remove various cruft from UI.

;; Users are encouraged to disable these cruft by using system configuration. Example,
;; tool bar can be turned off on X by setting `emacs.Toolbar: 0' in ~/.Xresources file,
;; and same can be done on MaxOS by the command
;; defaults write org.gnu.Emacs ToolBar -string no
;; defaults write org.gnu.Emacs ScrollBar -string no
(when (display-graphic-p)
  (when tool-bar-mode
    (tool-bar-mode -1))
  (scroll-bar-mode    -1))

(line-number-mode    1)
(column-number-mode  1)
(blink-cursor-mode  -1)
(setq read-file-name-completion-ignore-case t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)


;; Window management

;;Use shift-<left/right/up/down> to change windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

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


;; Theming

;; Add user specified theme.
(defcustom emacs-fast/theme '(material-theme . material)
  "The theme for Emacs-fast.
First argument is the name of theme package, which is downloaded
and installed.  Second argument is the name of the theme which
can be loaded with `load-theme'.  Some packages might install
multiple usable themes."
  :group 'emacs-fast
  :type '(cons
          (symbol :tag "Theme Package " :value 'material-theme)
          (symbol :tag "Theme name    " :value 'material)))

(defun emacs-fast/install-and-load-theme (package-name theme-name)
  "Install PACKAGE-NAME and load THEME-NAME from it."
  (eval `(use-package ,package-name
           :ensure t
           :no-require t
           :config
           (load-theme ',theme-name t))))

;; Don't load the theme if user has a predefined theme.
(unless custom-enabled-themes
  (emacs-fast/install-and-load-theme
   (car emacs-fast/theme)
   (cdr emacs-fast/theme)))

;; Modeline
(use-package all-the-icons
  :ensure t
  :demand t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)                  ; Show Icons
  (doom-modeline-minor-modes nil)         ; Minor modes
  (doom-modeline-env-version nil)         ; Environment details
  (doom-modeline-buffer-file-name-style 'truncate-with-project) ; Truncate file names
  (doom-modeline-buffer-encoding nil)     ; Buffer encoding
  (doom-modeline-checker-simple-format t) ; Single number for errors/warnings
  :hook
  (after-init . doom-modeline-mode))


;; Other niceties

;; Show key config for shortcuts.
(defvar which-key-idle-delay)
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
(diminish 'auto-fill-function "ⓐ")
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'visual-line-mode "⒱")

(provide 'femacs-ui)
;;; femacs-ui.el ends here
