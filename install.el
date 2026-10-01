;; -*- lexical-binding: t; -*-

;; Install the local package by opening this file and running M-x eval-buffer,
;; calling (load "/path/to/yandex-arc/install.el"), or placing point on this
;; file in Dired and pressing L (dired-do-load) with no other files marked.

(require 'package)
(require 'dired)

(let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
  (with-temp-buffer
    (dired-mode default-directory)
    (dired-readin)
    (dired-mark-files-regexp "^yandex-arc.*\\.el$")
    (package-install-from-buffer)))
