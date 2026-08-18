;;; femacs-coding-agents.el --- Coding agent integrations -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/emacs-fast

;;; Commentary:
;; Integrations with coding agents and local review annotations.
;;
;; Prerequisites:
;; - Emacs 28.1+
;; - Claude Code CLI installed (`npm install -g @anthropic/claude-code`) for
;;   claude-code-ide.el
;; - An ACP-compatible agent installed for agent-shell
;; - vterm or eat terminal emulator
;;
;; Usage:
;; - C-c a a : Start or resume an agent-shell session
;; - C-c a p : Compose an agent-shell prompt in a dedicated buffer
;; - C-c r a : Annotate a region or word in a diff with reviewer.el
;; - C-c r r : Render current-buffer annotations as Org text
;; - C-c a i : Open Claude Code IDE menu
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

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell" :protocol https)
  :bind (("C-c a a" . agent-shell)
         ("C-c a p" . agent-shell-prompt-compose))
  :custom
  ;; Compose prompts in a regular text buffer rather than at a comint prompt.
  (agent-shell-prefer-viewport-interaction t))

(use-package reviewer
  :straight (:host github :repo "SreenivasVRao/reviewer.el" :protocol https)
  :hook ((diff-mode . reviewer-mode)
         (magit-diff-mode . reviewer-mode)))

(provide 'femacs-coding-agents)
;;; femacs-coding-agents.el ends here
