;; -*- lexical-binding: t; -*-

(defun yandex-arc/actions/visit-file (file-name)
  "Visits file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (find-file file-name)))


(defun yandex-arc/actions/stage-file (file-name)
  "Stages file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (yandex-arc/shell/stage file-name)
    (yandex-arc/update-arc-buffer)))


(defun yandex-arc/actions/unstage-file (file-name)
  "Unstages file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (yandex-arc/shell/unstage file-name)
    (yandex-arc/update-arc-buffer)))
