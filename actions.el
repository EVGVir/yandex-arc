;; -*- lexical-binding: t; -*-

(defun yandex-arc/actions/not-implemented-message ()
  (interactive)
  (ding t)
  (message "¯\\_(ツ)_/¯   Not implemented yet"))


(defun yandex-arc/actions/visit-file (file-name)
  "Visits file at point."
  (interactive (list (magit-section-value-if 'magit-file-section)))
  (when file-name
    (find-file file-name)))


;; Staging
(defun yandex-arc/actions/stage-file ()
  "Stages file(s) at point."
  (interactive)
  (let ((file-names (yandex-arc/get-file-names-from-section-at-point)))
    (dolist (file-name file-names)
      (yandex-arc/shell/stage file-name))
    (when (length file-names)
      (yandex-arc/revert-arc-buffer nil nil))))


(defun yandex-arc/actions/unstage-file ()
  "Unstages file(s) at point."
  (interactive)
  (let ((file-names (yandex-arc/get-file-names-from-section-at-point)))
    (dolist (file-name file-names)
      (yandex-arc/shell/unstage file-name))
    (when (length file-names)
      (yandex-arc/revert-arc-buffer nil nil))))


;; Stashing
(transient-define-prefix yandex-arc/actions/stash-transient ()
  [["Stash"
    ("z" "both"     yandex-arc/actions/stash-push)
    ("i" "index"    yandex-arc/actions/not-implemented-message)
    ("w" "worktree" yandex-arc/actions/not-implemented-message)]
   ["Use"
    ("a" "Apply" yandex-arc/actions/stash-apply)
    ("p" "Pop"   yandex-arc/actions/not-implemented-message)
    ("k" "Drop"  yandex-arc/actions/not-implemented-message)]])


(defun yandex-arc/actions/stash-push (message)
  "Puts everything into the stash."
  (interactive "MStash message: ")
  (message (string-trim (yandex-arc/shell/stash-push message)))
  (yandex-arc/revert-arc-buffer nil nil))


(defun yandex-arc/actions/stash-apply ()
  "Applies the stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (progn
          (yandex-arc/shell/stash-apply stash-index t)
          (yandex-arc/revert-arc-buffer nil nil))
      (ding)
      (message "No stash selected."))))


(defun yandex-arc/actions/show-all-branches ()
  "Lists all branches."
  (interactive)
  (yandex-arc/branches/show-all-branches))
