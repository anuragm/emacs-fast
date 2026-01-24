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
