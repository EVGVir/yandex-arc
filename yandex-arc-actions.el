;; -*- lexical-binding: t; -*-

(provide 'yandex-arc-actions)

(require 'yandex-arc)
(require 'yandex-arc-branches)
(require 'yandex-arc-properties)
(require 'yandex-arc-revision)
(require 'yandex-arc-shell)

(require 'eieio)
(require 'magit-section)
(require 'transient)


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
      (revert-buffer))))


(defun yandex-arc/actions/unstage-file ()
  "Unstages file(s) at point."
  (interactive)
  (let ((file-names (yandex-arc/get-file-names-from-section-at-point)))
    (dolist (file-name file-names)
      (yandex-arc/shell/unstage file-name))
    (when (length file-names)
      (revert-buffer))))


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
  (message "%s" (slot-value (yandex-arc/shell/stash-push message :all) 'value))
  (revert-buffer))


(defun yandex-arc/actions/stash-push-worktree (message)
  "Pushes worktree changes into the stash."
  (interactive "MStash message: ")
  (message "%s" (slot-value (yandex-arc/shell/stash-push message :worktree) 'value))
  (revert-buffer))


(defun yandex-arc/actions/stash-apply ()
  "Applies stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (let ((result (yandex-arc/shell/stash-apply stash-index t)))
          (if (= (slot-value result 'return-code) 0)
              (revert-buffer)
            (ding t)
            (message "%s" (slot-value result 'value))))
      (ding)
      (message "No stash selected."))))


(defun yandex-arc/actions/stash-pop ()
  "Pops stash at point."
  (interactive)
  (let ((stash-index (magit-section-value-if 'yandex-arc/stash-section)))
    (if stash-index
        (let ((result (yandex-arc/shell/stash-pop stash-index t)))
          (if (= (slot-value result 'return-code) 0)
              (revert-buffer)
            (ding t)
            (message "%s" (slot-value result 'value))))
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
            (revert-buffer)))
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
    ("k" "delete"            yandex-arc/actions/delete-branch)
    ("m" "rename"            yandex-arc/actions/rename-branch)]])


(defun yandex-arc/actions/show-all-branches ()
  "Lists all branches."
  (interactive)
  (yandex-arc/branches/show-all-branches))


(defun yandex-arc/actions/create-new-branch (start-at branch-name)
  "Creates a new branch BRANCH-NAME.

Returns the code returned by `arc`."
  (interactive "sCreate branch starting at (default \"trunk\"): \nsName for new branch: ")
  (if (length= start-at 0) (setq start-at "trunk"))
  (let* ((result (yandex-arc/shell/branch-create start-at branch-name))
         (return-code (slot-value result 'return-code)))
    (when (/= return-code 0)
      (ding t)
      (message "%s" (slot-value result 'value)))
    return-code))


(defun yandex-arc/actions/checkout (branch-name-or-revision)
  "Checkouts BRANCH-NAME-OR-REVISION."
  (interactive
   (list (read-from-minibuffer
          "Checkout: "
          (yandex-arc/properties/get-branch-name-at-point))))

  (let ((result (yandex-arc/shell/checkout branch-name-or-revision)))
    (when (/= (slot-value result 'return-code) 0)
      (ding t)
      (message "%s" (slot-value result 'value))))
  (revert-buffer))


(defun yandex-arc/actions/create-and-checkout (start-at branch-name)
  "Creates and checkouts a new branch BRANCH-NAME."
  (interactive "sCreate and checkout branch starting at (default \"trunk\"): \nsName for new branch: ")
  (when (= (yandex-arc/actions/create-new-branch start-at branch-name) 0)
    (yandex-arc/actions/checkout branch-name)))


(defun yandex-arc/actions/delete-branch (branch-name)
  "Deletes branch BRANCH-NAME."
  (interactive
   (list (read-from-minibuffer
          "Delete branch: "
          (yandex-arc/properties/get-branch-name-at-point))))

  (let ((result (yandex-arc/shell/delete-branch branch-name)))
    (if (/= (slot-value result 'return-code) 0)
        (ding t)
      (message "%s" (slot-value result 'value))
      (when (eq major-mode 'yandex-arc-branches-mode)
        (revert-buffer)))))


(defun yandex-arc/actions/rename-branch (from to)
  "Renames branch with name FROM to name TO."
  (interactive
   (let* ((from (read-from-minibuffer "Rename branch: " (yandex-arc/properties/get-branch-name-at-point)))
          (to   (read-from-minibuffer (concat "Rename branch '" from "', to: "))))
     (list from to)))

  (let ((result (yandex-arc/shell/rename-branch from to)))
    (if (/= (slot-value result 'return-code) 0)
        (ding t)
      (revert-buffer))
    (message "%s" (slot-value result 'value))))


;; Commit
(transient-define-prefix yandex-arc/actions/commit-transient ()
  [["Create"
    ("c" "Commit" yandex-arc/actions/commit)]
   ["Edit HEAD"
    ("a" "Amend" yandex-arc/actions/amend)]])


(defun yandex-arc/actions/revert-buffer-on-process-exit (process event)
  "A process sentinel that redraws a buffer on PROCESS exit."
  (ignore event)
  (unless (process-live-p process)
    (when (/= (process-exit-status process) 0)
      (ding t))
    (revert-buffer)))


(defun yandex-arc/actions/commit-filter (process string)
  (ignore process)
  (when (string-match-p "nothing to commit!" string)
    (message "nothing to commit!")))


(defun yandex-arc/actions/commit ()
  (interactive)
  (yandex-arc/shell/commit
   'yandex-arc/actions/commit-filter
   'yandex-arc/actions/revert-buffer-on-process-exit))


(defun yandex-arc/actions/amend ()
  (interactive)
  (yandex-arc/shell/amend
   'yandex-arc/actions/commit-filter
   'yandex-arc/actions/revert-buffer-on-process-exit))


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
    (message "%s" (slot-value result 'value))
    (if (/= (slot-value result 'return-code) 0)
        (ding t)
      (revert-buffer))))


(defun yandex-arc/actions/pull-request-create-filter (process string)
  (ignore process)
  (message (yandex-arc/shell/normalize-string string)))


(defun yandex-arc/actions/pull-request-create ()
  (interactive)
  (yandex-arc/shell/pull-request-create
   'yandex-arc/actions/pull-request-create-filter
   'yandex-arc/actions/revert-buffer-on-process-exit))


;; Pull
(defun yandex-arc/actions/pull ()
  (interactive)
  (let ((result (yandex-arc/shell/pull)))
    (if (= (slot-value result 'return-code) 0)
        (revert-buffer)
      (ding t)
      (message "%s" (slot-value result 'value)))))


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
         (return-code (slot-value result 'return-code))
         (return-value (slot-value result 'value)))
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
      ((eq (magit-section-parent-value (magit-current-section)) :untracked)
       (yandex-arc/actions/delete-file (magit-section-value-if 'magit-file-section)))
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
    (revert-buffer)))


(defun yandex-arc/actions/delete-file (file)
  (when (yes-or-no-p (concat "Trash file \"" file "\""))
    (delete-file file)
    (revert-buffer)))
