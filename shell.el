;; -*- lexical-binding: t; -*-

(defvar-local yandex-arc/shell/arc-bin "arc"
  "Yandex Arc binary.")


(defvar-local yandex-arc/shell/root nil
  "Value returned by the most recent call to `arc root`.")


(defun yandex-arc/shell/run-arc (&rest args)
  (setq args (flatten-list args))
  (apply 'process-file (append (list yandex-arc/shell/arc-bin nil t nil) args)))


(defun yandex-arc/shell/run-arc-json (&rest args)
  (with-temp-buffer
    (setq args (append args '("--json")))
    (yandex-arc/shell/run-arc args)
    (goto-char 0)
    (json-parse-buffer)))


(defun yandex-arc/shell/run-arc-text (&rest args)
  (with-temp-buffer
    (yandex-arc/shell/run-arc args)
    (buffer-string)))


(defun yandex-arc/shell/update-root ()
  (setq yandex-arc/shell/root (string-trim (yandex-arc/shell/run-arc-text "root"))))


(defun yandex-arc/shell/status ()
  (yandex-arc/shell/run-arc-json "status"))


(defun yandex-arc/shell/info ()
  (yandex-arc/shell/run-arc-json "info"))


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


(defun yandex-arc/shell/stash-list ()
  "List available entries in the stash."
  (yandex-arc/shell/run-arc-json "stash" "list"))


(defun yandex-arc/shell/stash-push ()
  "Puts everything into the stash."
  (yandex-arc/shell/run-arc-text "stash" "push"))


(defun yandex-arc/shell/stash-apply (stash-num --index)
  "Apply stash entry under number STASH-NUM, not removing it from
stack. If --INDEX is t then index state is restored."
  (yandex-arc/shell/run-arc-text
   "stash" "apply" (number-to-string stash-num) (when --index "--index")))
