;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent package.el from loading packages before straight.el
(setq package-enable-at-startup nil)

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-deferred-compilation t))

;;; early-init.el ends here
