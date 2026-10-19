;; -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -l tests/run-tests.el

(require 'package)
(package-initialize)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name))))
      (load-prefer-newer t))
  (add-to-list 'load-path root)
  ;; Use the normal entry point so missing dependencies are not hidden
  ;; by preloading every module. Prefer sources over stale .elc files.
  (require 'yandex-arc)
  (dolist (file (directory-files (expand-file-name "tests" root) t "-test\\.el$"))
    (load file nil t)))

(ert-run-tests-batch-and-exit)
