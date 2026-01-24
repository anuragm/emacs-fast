;;; femacs-claude-ide.el --- Claude Code IDE integration -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast

;;; Commentary:
;; Integration with Claude Code CLI via claude-code-ide.el
;; Provides AI-assisted coding capabilities within Emacs.
;;
;; Prerequisites:
;; - Emacs 28.1+
;; - Claude Code CLI installed (`npm install -g @anthropic/claude-code`)
;; - vterm or eat terminal emulator
;;
;; Usage:
;; - C-c C-' : Open Claude Code IDE menu
;; - M-x claude-code-ide : Start new session in current project
;; - M-x claude-code-ide-send-prompt : Send prompt to active session
;; - M-x claude-code-ide-resume : Resume previous session
;; - M-x claude-code-ide-list-sessions : List all sessions

;;; License:
;; MIT License

;;; Code:

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c a i" . claude-code-ide-menu)
  :custom
  ;; Terminal backend (vterm or eat)
  (claude-code-ide-terminal-backend 'vterm)
  ;; Enable IDE diff viewer with ediff
  (claude-code-ide-use-ide-diff t)
  ;; Show in side window
  (claude-code-ide-use-side-window t)
  ;; Window position (right, left, top, bottom)
  (claude-code-ide-window-side 'right)
  ;; Anti-flicker for vterm rendering
  (claude-code-ide-vterm-anti-flicker t)
  ;; Optional: Enable MCP server for advanced features
  ;; (claude-code-ide-enable-mcp-server t)
  ;; Optional: Custom CLI flags
  ;; (claude-code-ide-cli-extra-flags "--model opus")
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'femacs-claude-ide)
;;; femacs-claude-ide.el ends here
