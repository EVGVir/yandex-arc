;; -*- lexical-binding: t; -*-

(defun yandex-arc/actions/not-implemented-message ()
  (interactive)
  (ding t)
  (message "¯\\_(ツ)_/¯   Not implemented yet"))


;; Actions at point
(defun yandex-arc/actions/visit-at-point ()
  (interactive)
  (let ((type (get-text-property (point) 'yandex-arc/properties/type-property)))
    (cond ((eq type 'file-name)
           (find-file (get-text-property (point) 'yandex-arc/properties/file-name-property)))
          ((eq type 'branch-name)
           (yandex-arc/actions/show-commit (get-text-property (point) 'yandex-arc/properties/branch-name-property)))
          ((eq type 'hash)
           (yandex-arc/actions/show-commit (get-text-property (point) 'yandex-arc/properties/hash-property)))
          ((eq type 'link)
           (browse-url (get-text-property (point) 'button-data))))))


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
    ("w" "worktree" yandex-arc/actions/stash-push-worktree)]
   ["Use"
    ("a" "Apply" yandex-arc/actions/stash-apply)
    ("p" "Pop"   yandex-arc/actions/stash-pop)
    ("k" "Drop"  yandex-arc/actions/stash-drop)]])


(defun yandex-arc/actions/stash-push (message)
  "Pushes all changes into the stash."
  (interactive "MStash message: ")
  (message "%s" (oref (yandex-arc/shell/stash-push message :all) :value))
  (yandex-arc/revert-arc-buffer nil nil))


(defun yandex-arc/actions/stash-push-worktree (message)
  "Pushes worktree changes into the stash."
  (interactive "MStash message: ")
  (message "%s" (oref (yandex-arc/shell/stash-push message :worktree) :value))
  (yandex-arc/revert-arc-buffer nil nil))


(defun yandex-arc/actions/stash-apply ()
  "Applies stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (progn
          (yandex-arc/shell/stash-apply stash-index t)
          (yandex-arc/revert-arc-buffer nil nil))
      (ding)
      (message "No stash selected."))))


(defun yandex-arc/actions/stash-pop ()
  "Pops stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (progn
          (yandex-arc/shell/stash-pop stash-index t)
          (yandex-arc/revert-arc-buffer nil nil))
      (ding)
      (message "No stash selected."))))


(defun yandex-arc/actions/stash-drop ()
  "Drops stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (when (yes-or-no-p (concat "Drop stash@{" (number-to-string stash-index) "}?"))
          (progn
            (yandex-arc/shell/stash-drop stash-index)
            (yandex-arc/revert-arc-buffer nil nil)))
      (ding)
      (message "No stash selected."))))


;; Branches
(transient-define-prefix yandex-arc/actions/branch-transient ()
  [["Checkout"
    ("b" "branch/revision"   yandex-arc/actions/checkout)
    ("c" "new branch"        yandex-arc/actions/create-and-checkout)
    ("p" "pull request"      yandex-arc/actions/pull-request-checkout)]
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


;; Commit
(transient-define-prefix yandex-arc/actions/commit-transient ()
  [["Create"
    ("c" "Commit" yandex-arc/actions/commit)]
   ["Edit HEAD"
    ("a" "Amend" yandex-arc/actions/amend)]])


(defun yandex-arc/actions/revert-arc-buffer-on-process-exit (process event)
  "A process sentinel that redraws a buffer on PROCESS exit."
  (unless (process-live-p process)
    (when (/= (process-exit-status process) 0)
      (ding t))
    (yandex-arc/revert-arc-buffer nil nil)))


(defun yandex-arc/actions/commit-filter (process string)
  (when (string-match-p "nothing to commit!" string)
    (message "nothing to commit!")))


(defun yandex-arc/actions/commit ()
  (interactive)
  (yandex-arc/shell/commit
   'yandex-arc/actions/commit-filter
   'yandex-arc/actions/revert-arc-buffer-on-process-exit))


(defun yandex-arc/actions/amend ()
  (interactive)
  (yandex-arc/shell/amend
   'yandex-arc/actions/commit-filter
   'yandex-arc/actions/revert-arc-buffer-on-process-exit))


;; Pull request
(transient-define-prefix yandex-arc/actions/pull-request-transient ()
  [["Checkout"
    ("co" "checkout" yandex-arc/actions/pull-request-checkout)]
   ["Create"
    ("cr" "create" yandex-arc/actions/pull-request-create)]])


(defun yandex-arc/actions/pull-request-checkout (id)
  "Checkouts pull request with the specified ID."
  (interactive "nCheckout pull request: ")
  (let ((result (yandex-arc/shell/pull-request-checkout id)))
    (message "%s" (oref result :value))
    (if (/= (oref result :return-code) 0)
        (ding t)
      (yandex-arc/revert-arc-buffer nil nil))))


(defun yandex-arc/actions/pull-request-create-filter (process string)
  (message (yandex-arc/shell/normalize-string string)))


(defun yandex-arc/actions/pull-request-create ()
  (interactive)
  (yandex-arc/shell/pull-request-create
   'yandex-arc/actions/pull-request-create-filter
   'yandex-arc/actions/revert-arc-buffer-on-process-exit))


;; Pull
(defun yandex-arc/actions/pull ()
  (interactive)
  (yandex-arc/shell/pull)
  (yandex-arc/revert-arc-buffer nil nil))


;; Push
(transient-define-prefix yandex-arc/actions/push-transient ()
  ["Arguments"
   ("-f" "force"                             "--force")
   ("-n" "disable the pre-push hook"         "--no-verify")
   ("-p" "publish the latest diff-set in PR" "--publish")]
  ["Actions"
   ("p" "push" yandex-arc/actions/push)])


(defun yandex-arc/actions/push ()
  (interactive)
  (let* ((args (transient-args 'yandex-arc/actions/push-transient))
         (force     (transient-arg-value "--force"     args))
         (no-verify (transient-arg-value "--no-verify" args))
         (publish   (transient-arg-value "--publish"   args))
         (result (yandex-arc/shell/push force no-verify publish))
         (return-code (oref result :return-code))
         (return-value (oref result :value)))
    (message "%s" return-value)
    (when (/= return-code 0) (ding))))

;; Diff
(transient-define-prefix yandex-arc/actions/diff-transient ()
  [["Actions"
    ("c" "Show commit" yandex-arc/actions/show-commit)]])


(defun yandex-arc/actions/show-commit (commit)
  (interactive "sShow commit: ")
  (yandex-arc/revision/show-revision commit))


;; Discard
(defun yandex-arc/actions/discard-at-point ()
  (interactive)
  (magit-section-case
    ('yandex-arc/files-section
     (yandex-arc/actions/not-implemented-message))
    ('magit-file-section
     (cond
      ((eq (magit-section-parent-value (magit-current-section)) :unstaged)
       (yandex-arc/actions/discard-file (magit-section-value-if 'magit-file-section)))
      (t
       (yandex-arc/actions/not-implemented-message))))
    ('yandex-arc/stashes-section
     (yandex-arc/actions/not-implemented-message))
    ('yandex-arc/stash-section
     (yandex-arc/actions/stash-drop))))


(defun yandex-arc/actions/discard-file (file)
  (when (yes-or-no-p (concat "Discard unstaged changes in " file))
    (yandex-arc/shell/discard-file file)
    (yandex-arc/revert-arc-buffer nil nil)))
