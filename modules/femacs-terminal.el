;;; femacs-terminal.el --- Terminal emulator support -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast

;;; Commentary:
;; Terminal emulator for Claude Code IDE and general use.
;; Provides vterm as the default terminal, with eat as a fallback option.

;;; License:
;; MIT License

;;; Code:

;; Enable terminal mouse reporting without affecting graphical frames.
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<wheel-up>") 'scroll-down-line)
  (global-set-key (kbd "<wheel-down>") 'scroll-up-line))

;; Terminal protocols have no standard Super modifier.  Decode two otherwise
;; unused sequences as Super arrows so the UI module's Windmove bindings work
;; over SSH as well as in graphical Emacs.
;;
;; iTerm2: Settings > Profiles > Keys > Key Mappings.  Add Option+Left and
;; Option+Right with the "Send Escape Sequence" action and values "[99;1D"
;; and "[99;1C", respectively.  Other terminal emulators can use the same
;; escape sequences in their custom key mappings.
(define-key input-decode-map "\e[99;1D" [s-left])
(define-key input-decode-map "\e[99;1C" [s-right])

;; vterm - Native terminal emulator (recommended for performance)
(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t))

;; eat - Pure Elisp terminal (uncomment if vterm compilation fails)
;; (use-package eat
;;   :commands (eat eat-other-window))

(provide 'femacs-terminal)
;;; femacs-terminal.el ends here
