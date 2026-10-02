;; -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -l tests/run-tests.el

(require 'package)
(package-initialize)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name))))
      (load-prefer-newer t))
  (add-to-list 'load-path root)
  ;; Explicit loads ensure tests use source even if old .elc files exist.
  (dolist (file (directory-files root t "^yandex-arc.*\\.el$"))
    ;; Package metadata is read by package.el, not loaded as library code.
    (unless (string-suffix-p "-pkg.el" file)
      (load file nil t)))
  (dolist (file (directory-files (expand-file-name "tests" root) t "-test\\.el$"))
    (load file nil t)))

(ert-run-tests-batch-and-exit)
