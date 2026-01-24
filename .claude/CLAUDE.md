# Emacs Configuration Project

This is a modular Emacs configuration using straight.el and use-package.

## Architecture Reference

When investigating repository structure, adding new modules, or answering questions about how this configuration is organized, read `.claude/ARCHITECTURE.md`.

## Development

- Modules live in `modules/` directory
- Language-specific modules are in `modules/lang/`
- Follow the `femacs-<feature>.el` naming convention
- Use `use-package` with straight.el for package management
