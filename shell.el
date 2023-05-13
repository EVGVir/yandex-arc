;; -*- lexical-binding: t; -*-

(defvar-local yandex-arc/shell/arc-bin "arc"
  "Yandex Arc binary.")


(defclass yandex-arc/arc-result ()
  ((return-code :initarg :return-code)
   (value :initarg :value))
  "Result returned by `arc` command either in text or JSON formats.")


(defun yandex-arc/shell/run-arc (&rest args)
  (setq args (flatten-list args))
  (apply 'process-file (append (list yandex-arc/shell/arc-bin nil t nil) args)))


(defun yandex-arc/shell/run-arc-json (&rest args)
  (with-temp-buffer
    (setq args (append args '("--json")))
    (let ((return-code (yandex-arc/shell/run-arc args)))
      (goto-char 0)
      (yandex-arc/arc-result
       :return-code return-code
       :value (json-parse-buffer)))))


(defun yandex-arc/shell/run-arc-text (&rest args)
  (with-temp-buffer
    (let ((return-code (yandex-arc/shell/run-arc args)))
      (yandex-arc/arc-result
       :return-code return-code
       :value (string-trim (buffer-string))))))


(defun yandex-arc/shell/root ()
  (yandex-arc/shell/run-arc-text "root"))


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


(defun yandex-arc/shell/stash-push (message)
  "Puts everything into the stash."
  (if (string-empty-p message)
      (yandex-arc/shell/run-arc-text "stash" "push")
    (yandex-arc/shell/run-arc-text "stash" "push" "-m" message)))


(defun yandex-arc/shell/stash-apply (stash-num --index)
  "Apply stash entry under number STASH-NUM, not removing it from
stack. If --INDEX is t then index state is restored."
  (yandex-arc/shell/run-arc-text
   "stash" "apply" (number-to-string stash-num) (when --index "--index")))


(defun yandex-arc/shell/branch-list ()
  (yandex-arc/shell/run-arc-json "branch" "--list" "--all"))


(defun yandex-arc/shell/branch-create (start-at branch-name)
  (yandex-arc/shell/run-arc-text "branch" branch-name start-at))
