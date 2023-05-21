;; -*- lexical-binding: t; -*-


(defclass yandex-arc/revision/summary-section (magit-section) ())
(defclass yandex-arc/revision/message-section (magit-section) ())


(defvar-local yandex-arc/revision/commit nil
  "Commit the revision buffer shows information about.")


(define-derived-mode yandex-arc-revision-mode magit-section-mode "arc-revision"
  "Yandex Arc Revision Major Mode."
  (setq-local revert-buffer-function
              (lambda (ignore-auto noconfirm) (yandex-arc/revision/redraw-buffer))))


(defvar-keymap yandex-arc-revision-mode-map
  :doc "Keymap for `yandex-arc-revision-mode'."
  "<return>" 'yandex-arc/actions/visit-at-point)


(defun yandex-arc/revision/show-revision (commit)
  (let ((buffer (get-buffer-create (concat "arc-revision: " (file-name-nondirectory default-directory)))))
    (set-buffer buffer)
    (yandex-arc-revision-mode)
    (setq-local yandex-arc/revision/commit commit)
    (yandex-arc/revision/redraw-buffer)
    (switch-to-buffer buffer)))


(defun yandex-arc/revision/redraw-buffer ()
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local header-line-format yandex-arc/revision/commit)
      (magit-insert-section (yandex-arc/root-section)
        (when (not (yandex-arc/revision/is-stash yandex-arc/revision/commit))
          (let ((description (yandex-arc/revision/get-commit-description yandex-arc/revision/commit)))
            (yandex-arc/revision/insert-summary-section description)
            (yandex-arc/revision/insert-message-section description)))
        (yandex-arc/insert-files-section
         "Changes"
         (yandex-arc/revision/diff-commits-file-names-only yandex-arc/revision/commit)
         :commit
         yandex-arc/revision/commit)))))


(defun yandex-arc/revision/is-stash (commit)
  (string-match-p "^stash@{[[:digit:]]+}" commit))


(defun yandex-arc/revision/get-commit-description (commit)
  (let ((description (yandex-arc/shell/log-describe-commit commit)))
    (cond ((/= (oref description :return-code) 0)
           (user-error "%s" (oref description :value)))
          ((= (length (oref description :value)) 0)
           (user-error "There is no information about commit \'%s\'." commit))
          (t ; No errors
           (elt (oref description :value) 0)))))


(defun yandex-arc/revision/diff-commits-file-names-only (commit)
  (let ((result (yandex-arc/shell/diff-commits-file-names-only (concat commit "^") commit)))
    (cond ((/= (oref result :return-code) 0)
           (user-error "%s" (oref result :value)))
          (t ; No errors
           (seq-map
            (lambda (file-name) (yandex-arc/shell/normalize-string file-name))
            (split-string (oref result :value)))))))


(defun yandex-arc/revision/insert-summary-section (description)
  (magit-insert-section (yandex-arc/revision/summary-section)
    (yandex-arc/revision/print-local-branches description)
    (let ((commit (gethash "commit" description))
          (author (gethash "author" description))
          (date   (gethash "date" description))
          (revision (gethash "revision" description)))
      (insert (yandex-arc/properties/hash commit) ?\n)
      (insert "Author:       "
              (yandex-arc/properties/link author "https://staff.yandex-team.ru/%s" author)
              ?\n)
      (insert "Date:         " date ?\n)
      (when revision
        (setq revision (number-to-string revision))
        (insert "SVN Revision: "
                (yandex-arc/properties/link revision "https://a.yandex-team.ru/arcadia/commit/r%s" revision)
                ?\n)))
    (yandex-arc/revision/print-attributes description)
    (insert ?\n)))


(defun yandex-arc/revision/print-local-branches (description)
  (let ((branches (gethash "branches" description)))
    (when (gethash "head" branches)
      (insert (yandex-arc/properties/branch-name "HEAD") " "))
    (seq-doseq (branch (gethash "local" branches))
      (insert (yandex-arc/properties/branch-name branch) " "))))


(defun yandex-arc/revision/print-attributes (description)
  (let ((attributes (gethash "attributes" description)))
    (when attributes
      (let ((pull-request-id (gethash "pr.id" attributes)))
        (insert "Pull request: "
                (yandex-arc/properties/link pull-request-id "https://a.yandex-team.ru/review/%s" pull-request-id)
                ?\n)))))


(defun yandex-arc/revision/insert-message-section (description)
  (magit-insert-section (yandex-arc/revision/message-section)
    (insert (gethash "message" description) ?\n)
    (insert ?\n)))
