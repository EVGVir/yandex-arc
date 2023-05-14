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
  (message "%s" (oref (yandex-arc/shell/stash-push message) :value))
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


;; Branches
(transient-define-prefix yandex-arc/actions/branch-transient ()
  [["Checkout"
    ("b" "branch/revision"   yandex-arc/actions/checkout)
    ("c" "new branch"        yandex-arc/actions/create-and-checkout)]
   ["Create"
    ("n" "new branch"        yandex-arc/actions/create-new-branch)]
   ["List"
    ("l" "list all branches" yandex-arc/actions/show-all-branches)]
   ["Do"
    ("k" "delete"            yandex-arc/actions/delete-branch)]])


(defun yandex-arc/actions/show-all-branches ()
  "Lists all branches."
  (interactive)
  (yandex-arc/branches/show-all-branches))


(defun yandex-arc/actions/create-new-branch (start-at branch-name)
  "Creates a new branch BRANCH-NAME.

Returns the code returned by `arc`."
  (interactive "sCreate branch starting at: \nsName for new branch: ")
  (let* ((result (yandex-arc/shell/branch-create start-at branch-name))
         (return-code (oref result :return-code)))
    (when (/= return-code 0)
      (ding t)
      (message "%s" (oref result :value)))
    return-code))


(defun yandex-arc/actions/checkout (branch-name-or-revision)
  "Checkouts BRANCH-NAME-OR-REVISION."
  (interactive "sCheckout: ")
  (let ((result (yandex-arc/shell/checkout branch-name-or-revision)))
    (when (/= (oref result :return-code) 0)
      (ding t)
      (message "%s" (oref result :value))))
  (yandex-arc/revert-arc-buffer nil nil))


(defun yandex-arc/actions/create-and-checkout (start-at branch-name)
  "Creates and checkouts a new branch BRANCH-NAME."
  (interactive "sCreate and checkout branch starting at: \nsName for new branch: ")
  (when (= (yandex-arc/actions/create-new-branch start-at branch-name) 0)
    (yandex-arc/actions/checkout branch-name)))


(defun yandex-arc/actions/delete-branch (branch-name)
  "Deletes branch BRANCH-NAME."
  (interactive "sDelete branch: ")
  (let ((result (yandex-arc/shell/delete-branch branch-name)))
    (when (/= (oref result :return-code) 0) (ding t))
    (message "%s" (oref result :value))))
