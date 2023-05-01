;; -*- lexical-binding: t; -*-

(defvar-local yandex-arc/arc-bin "arc"
  "Yandex Arc binary.")


(defvar-local yandex-arc/status-hash nil
  "Value returned by the most recent call to `arc status`.")


(defvar-local yandex-arc/info-hash nil
  "Value returned by the most recent call to `arc info`.")


(defvar-local yandex-arc/root nil
  "Value returned by the most recent call to `arc root`")


(defun yandex-arc/run-arc-json (&rest args)
  (with-temp-buffer
    (apply 'process-file (append (list yandex-arc/arc-bin nil t nil) args '("--json")))
    (goto-char 0)
    (json-parse-buffer)))


(defun yandex-arc/run-arc-text (&rest args)
  (with-temp-buffer
    (apply 'process-file (append (list yandex-arc/arc-bin nil t nil) args))
    (goto-char 0)
    (buffer-string)))


(defun yandex-arc/update-status ()
  (setq yandex-arc/status-hash (yandex-arc/run-arc-json "status")))


(defun yandex-arc/update-info ()
  (setq yandex-arc/info-hash (yandex-arc/run-arc-json "info")))


(defun yandex-arc/update-root ()
  (setq yandex-arc/root (string-trim (yandex-arc/run-arc-text "root"))))


(defun yandex-arc/stage (file-name)
  (yandex-arc/run-arc-text "add" file-name))


(defun yandex-arc/unstage (file-name)
  (yandex-arc/run-arc-text "reset" "HEAD" file-name))
