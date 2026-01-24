# Emacs Configuration Architecture

This document describes the structure of this Emacs configuration for future reference.

## Package Management

- **Primary**: straight.el (version 7+)
- **Configuration**: use-package macros
- **Protocol**: SSH by default for git operations
- **Verification**: Checks for modifications on save

## Directory Structure

```
~/.emacs.d/
├── init.el              # Main entry point (bootstraps straight.el, loads modules)
├── early-init.el        # Early initialization (GC tuning, disables package.el)
├── modules/             # Feature modules
│   ├── femacs-core.el         # Core utilities, file management
│   ├── femacs-ui.el           # UI elements, theming (material-theme, doom-modeline)
│   ├── femacs-editor.el       # Editor enhancements (line numbers, rainbow delimiters)
│   ├── femacs-autocomplete.el # Company, yasnippet, helm-company
│   ├── femacs-helm.el         # Helm completion/navigation
│   ├── femacs-helm-ag.el      # Helm ag integration
│   ├── femacs-lsp.el          # LSP mode, lsp-ui, treemacs
│   ├── femacs-scm.el          # Magit, git-link, forge, git-timemachine
│   ├── femacs-check.el        # Flyspell, flycheck, ispell
│   ├── femacs-misc.el         # Persistent scratch
│   ├── femacs-org.el          # Org mode with export support
│   ├── femacs-treesitter.el   # Tree-sitter support
│   ├── femacs-hydra.el        # Hydra key bindings
│   ├── femacs-lang.el         # Language module loader
│   ├── femacs-osx.el          # macOS-specific (iTerm, Finder integration)
│   ├── femacs-terminal.el     # Terminal emulator (vterm)
│   ├── femacs-claude-ide.el   # Claude Code IDE integration
│   └── lang/                  # Language-specific modules
│       ├── femacs-lang-python.el
│       ├── femacs-lang-cpp.el
│       ├── femacs-lang-cmake.el
│       ├── femacs-lang-latex.el
│       ├── femacs-lang-json.el
│       ├── femacs-lang-markdown.el
│       ├── femacs-lang-shell.el
│       ├── femacs-lang-elisp.el
│       ├── femacs-lang-matlab.el
│       ├── femacs-lang-cuda.el
│       ├── femacs-lang-csharp.el
│       └── femacs-lang-misc.el
├── private/             # User-specific configuration (not in git)
│   └── custom.el        # Emacs customize settings
└── straight/            # straight.el package directory (generated)
```

## Module Loading Order

Modules are loaded in `init.el` in this order:

1. `femacs-osx` (macOS only)
2. `femacs-hydra`
3. `femacs-core`
4. `femacs-ui`
5. `femacs-editor`
6. `femacs-check`
7. `femacs-autocomplete`
8. `femacs-helm`
9. `femacs-scm`
10. `femacs-helm-ag`
11. `femacs-misc`
12. `femacs-org`
13. `femacs-lsp`
14. `femacs-treesitter`
15. `femacs-lang` (loads all language modules)
16. `femacs-terminal`
17. `femacs-claude-ide`

## Emacs Version Requirements

- **Minimum**: Emacs 26.1
- **Recommended**: Emacs 28.1+ (for native compilation and claude-code-ide)

## Key Integrations

| Feature | Package | Module |
|---------|---------|--------|
| Completion | Company, Helm | femacs-autocomplete, femacs-helm |
| LSP | lsp-mode, lsp-ui | femacs-lsp |
| Git | Magit, Forge | femacs-scm |
| Linting | Flycheck | femacs-check |
| Syntax | Tree-sitter | femacs-treesitter |
| AI | claude-code-ide | femacs-claude-ide |
| Terminal | vterm | femacs-terminal |

## Adding New Modules

1. Create `modules/femacs-<name>.el`
2. Follow the template:
   ```elisp
   ;;; femacs-<name>.el --- Description -*- lexical-binding: t -*-
   ;;; Commentary:
   ;;; Code:

   (use-package <package>
     :config
     ...)

   (provide 'femacs-<name>)
   ;;; femacs-<name>.el ends here
   ```
3. Add `(require 'femacs-<name>)` to `init.el`

## Naming Conventions

- Modules: `femacs-<feature>.el`
- Language modules: `femacs-lang-<language>.el`
- Custom functions: `femacs/<feature>-<action>`
- Custom variables: `femacs/<feature>-<setting>`

---

*Last updated: 2026-01-24*
