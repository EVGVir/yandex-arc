;; -*- lexical-binding: t; -*-

(defun yandex-arc/actions/not-implemented-message ()
  (interactive)
  (ding t)
  (message "¯\\_(ツ)_/¯   Not implemented yet"))


(defun yandex-arc/actions/visit-file (file-name)
  "Visits file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (find-file file-name)))


;; Staging
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


(defun yandex-arc/actions/stash-push ()
  "Puts everything into the stash."
  (interactive)
  (message (string-trim (yandex-arc/shell/stash-push)))
  (yandex-arc/update-arc-buffer))


(defun yandex-arc/actions/stash-apply ()
  "Applies the stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (progn
          (yandex-arc/shell/stash-apply stash-index t)
          (yandex-arc/update-arc-buffer))
      (ding)
      (message "No stash selected."))))
