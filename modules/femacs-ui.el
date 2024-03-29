;;; femacs-ui.el --- Adds UI elements.
;;
;; Copyright © 2016-2021 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds various UI elements, including theme.

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

;; Highlight the current line in all buffers
(require 'hl-line)
(global-hl-line-mode +1)
(setq global-hl-line-sticky-flag t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Add the current working directory to frame title.
(setq frame-title-format '((:eval default-directory)))


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
  :commands all-the-icons-faicon
  :if window-system
  :custom
  (all-the-icons-scale-factor 1.1)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

;; Nerd icons required for doom modeline
(use-package nerd-icons
  :ensure t
  :config
  (unless (member nerd-icons-font-family (font-family-list))
    (nerd-icons-install-fonts)))

(use-package minions
  :ensure t
  :custom
  (minions-mode-line-lighter (all-the-icons-faicon "clipboard" :v-adjust 0 :height 0.83))
  :commands minions-mode)

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon nil)                ; Turn off icons by default.
  (doom-modeline-minor-modes t)           ; Minor modes
  (doom-modeline-env-version nil)         ; Environment details
  (doom-modeline-buffer-file-name-style 'truncate-with-project) ; Truncate file names
  (doom-modeline-buffer-encoding nil)     ; Buffer encoding
  (doom-modeline-checker-simple-format nil) ; Show error/warning/info
  :hook
  (after-init . doom-modeline-mode)
  :config
  (set-face-attribute
   'doom-modeline-panel nil :inverse-video t)
  (minions-mode 1)                        ; Minion mode for minor modes.
  (when (display-graphic-p)               ; Show icons on window systems.
    (setq doom-modeline-icon t)))


;; Other niceties

;; Show key config for shortcuts.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5)
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
