;; -*- lexical-binding: t; -*-

(provide 'yandex-arc-shell)

(require 'eieio)
(require 'with-editor)


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


(defun yandex-arc/shell/run-arc-with-editor (process-filter on-process-status-change &rest args)
  (with-editor
    (make-process
     :name "arc"
     :buffer nil
     :command (append (list yandex-arc/shell/arc-bin) args)
     :filter process-filter
     :sentinel on-process-status-change
     :file-handler t)))


(defun yandex-arc/shell/normalize-string (str)
  (replace-regexp-in-string
   "" "\n"
   (ansi-color-filter-apply (string-trim str))))


(defun yandex-arc/shell/run-arc-text (&rest args)
  (with-temp-buffer
    (let ((return-code (yandex-arc/shell/run-arc args)))
      (yandex-arc/arc-result
       :return-code return-code
       :value (yandex-arc/shell/normalize-string (buffer-string))))))


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


(defun yandex-arc/shell/diff-file (file-name diff-type &optional commit)
  "Returns diff for FILE as a string. DIFF-TYPE controls the diff type:
* :staged   - diff between HEAD and index.
* :unstaged - diff between working tree and index.
* :commit   - diff between COMMIT^..COMMIT."
  (cond
   ((eq diff-type :staged)   (yandex-arc/shell/diff-staged   file-name))
   ((eq diff-type :unstaged) (yandex-arc/shell/diff-unstaged file-name))
   ((eq diff-type :commit)   (yandex-arc/shell/diff-commits  (concat commit "^") commit file-name))))


(defun yandex-arc/shell/diff-unstaged (file-name)
  "Returns diff between working tree and index."
  (yandex-arc/shell/run-arc-text "diff" "--color" "never" file-name))


(defun yandex-arc/shell/diff-staged (file-name)
  "Returns diff index and HEAD."
  (yandex-arc/shell/run-arc-text "diff" "--color" "never" "--cached" file-name))


(defun yandex-arc/shell/diff-commits-file-names-only (from to)
  "Returns list of files diff FROM..TO."
  (yandex-arc/shell/run-arc-text "diff" from to "--name-only"))


(defun yandex-arc/shell/diff-commits (from to file-name)
  "Returns list of files diff FROM..TO."
  (yandex-arc/shell/run-arc-text "diff" "--no-color" from to file-name))


(defun yandex-arc/shell/stash-list ()
  "List available entries in the stash."
  (yandex-arc/shell/run-arc-json "stash" "list"))


(defun yandex-arc/shell/stash-push (message mode)
  "Pushes changes into the stash. MODE defines changes to be pushed:
* :all      - push all changes (worktree and index).
* :worktree - push worktree changes only."
  (if (string-empty-p message)
      (yandex-arc/shell/run-arc-text "stash" "push")
    (cond
     ((eq mode :all)      (yandex-arc/shell/run-arc-text "stash" "push" "-m" message))
     ((eq mode :worktree) (yandex-arc/shell/run-arc-text "stash" "push" "--keep-index" "-m" message)))))


(defun yandex-arc/shell/stash-apply (stash-num restore-index-state)
  "Applies stash entry under the number STASH-NUM, not removing
it from stack.

If RESTORE-INDEX-STATE is t then index state is restored."
  (yandex-arc/shell/run-arc-text
   "stash" "apply" (number-to-string stash-num) (when restore-index-state "--index")))


(defun yandex-arc/shell/stash-pop (stash-num restore-index-state)
  "Pops stash entry under the number STASH-NUM.

If RESTORE-INDEX-STATE is t then index state is restored."
  (yandex-arc/shell/run-arc-text
   "stash" "pop" (number-to-string stash-num) (when restore-index-state "--index")))


(defun yandex-arc/shell/stash-drop (stash-num)
  "Drops stash entry under the number STASH-NUM."
  (yandex-arc/shell/run-arc-text
   "stash" "drop" (number-to-string stash-num)))


(defun yandex-arc/shell/branch-list ()
  (yandex-arc/shell/run-arc-json "branch" "--list" "--all"))


(defun yandex-arc/shell/branch-create (start-at branch-name)
  (yandex-arc/shell/run-arc-text "branch" branch-name start-at))


(defun yandex-arc/shell/delete-branch (branch-name)
  (yandex-arc/shell/run-arc-text "branch" "--delete" branch-name))


(defun yandex-arc/shell/rename-branch (from to)
  (yandex-arc/shell/run-arc-text "branch" "--move" from to))


(defun yandex-arc/shell/checkout (branch-name-or-revision)
  (yandex-arc/shell/run-arc-text "checkout" branch-name-or-revision))


(defun yandex-arc/shell/commit (process-filter on-process-status-change)
  (yandex-arc/shell/run-arc-with-editor process-filter on-process-status-change "commit"))


(defun yandex-arc/shell/amend (process-filter on-process-status-change)
  (yandex-arc/shell/run-arc-with-editor process-filter on-process-status-change "commit" "--amend"))


(defun yandex-arc/shell/pull-request-checkout (id)
  (yandex-arc/shell/run-arc-text "pr" "checkout" (number-to-string id)))


(defun yandex-arc/shell/pull-request-create (process-filter on-process-status-change)
  (yandex-arc/shell/run-arc-with-editor process-filter on-process-status-change "pr" "create"))


(defun yandex-arc/shell/pull ()
  (yandex-arc/shell/run-arc-text "pull"))


(defun yandex-arc/shell/push (force no-verify publish)
  (let ((args (list "push")))
    (if force     (setq args (append args '("--force"))))
    (if no-verify (setq args (append args '("--no-verify"))))
    (if publish   (setq args (append args '("--publish"))))
    (apply 'yandex-arc/shell/run-arc-text args)))


(defun yandex-arc/shell/log-describe-commit (commit)
  (yandex-arc/shell/run-arc-json "log" commit "--max-count" "1"))


(defun yandex-arc/shell/discard-file (file)
  (yandex-arc/shell/run-arc-text "checkout" file))
