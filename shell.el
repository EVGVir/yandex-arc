;; -*- lexical-binding: t; -*-

(defvar-local yandex-arc/shell/arc-bin "arc"
  "Yandex Arc binary.")


(defvar-local yandex-arc/shell/status-hash nil
  "Value returned by the most recent call to `arc status`.")


(defvar-local yandex-arc/shell/info-hash nil
  "Value returned by the most recent call to `arc info`.")


(defvar-local yandex-arc/shell/root nil
  "Value returned by the most recent call to `arc root`")


(defun yandex-arc/shell/run-arc-json (&rest args)
  (with-temp-buffer
    (apply 'process-file (append (list yandex-arc/shell/arc-bin nil t nil) args '("--json")))
    (goto-char 0)
    (json-parse-buffer)))


(defun yandex-arc/shell/run-arc-text (&rest args)
  (with-temp-buffer
    (apply 'process-file (append (list yandex-arc/shell/arc-bin nil t nil) args))
    (goto-char 0)
    (buffer-string)))


(defun yandex-arc/shell/update-status ()
  (setq yandex-arc/shell/status-hash (yandex-arc/shell/run-arc-json "status")))


(defun yandex-arc/shell/update-info ()
  (setq yandex-arc/shell/info-hash (yandex-arc/shell/run-arc-json "info")))


(defun yandex-arc/shell/update-root ()
  (setq yandex-arc/shell/root (string-trim (yandex-arc/shell/run-arc-text "root"))))


(defun yandex-arc/shell/stage (file-name)
  (yandex-arc/shell/run-arc-text "add" file-name))


(defun yandex-arc/shell/unstage (file-name)
  (yandex-arc/shell/run-arc-text "reset" "HEAD" file-name))


(defun yandex-arc/shell/diff-file (file-name diff-type)
  "Returns diff for FILE as a string. DIFF-TYPE controls the diff type:
* :staged   - diff between HEAD and index.
* :unstaged - diff between working tree and index."
  (cond
   ((eq diff-type :staged)   (yandex-arc/shell/diff-staged   file-name))
   ((eq diff-type :unstaged) (yandex-arc/shell/diff-unstaged file-name))))


(defun yandex-arc/shell/diff-unstaged (file-name)
  "Returns diff between working tree and index."
  (yandex-arc/shell/run-arc-text "diff" "--color" "never" file-name))


(defun yandex-arc/shell/diff-staged (file-name)
  "Returns diff index and HEAD."
  (yandex-arc/shell/run-arc-text "diff" "--color" "never" "--cached" file-name))
