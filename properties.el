;; -*- lexical-binding: t; -*-


(defun yandex-arc/properties/branch-name (branch-name)
  (propertize branch-name 'font-lock-face 'magit-branch-local))


(defun yandex-arc/properties/current-branch-name (branch-name)
  (propertize branch-name 'font-lock-face 'magit-branch-current))


(defun yandex-arc/properties/section-heading (section-name)
  (propertize section-name 'font-lock-face 'magit-section-heading))


(defun yandex-arc/properties/diff-file-heading (file-name)
  (propertize file-name 'font-lock-face 'magit-diff-file-heading))


(defun yandex-arc/properties/file-name (file-name)
  (propertize file-name 'font-lock-face 'magit-filename))


(defun yandex-arc/properties/hash (hash)
  (propertize hash 'font-lock-face 'magit-hash))
