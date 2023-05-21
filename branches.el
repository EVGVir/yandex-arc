;; -*- lexical-binding: t; -*-


(defclass yandex-arc/branches-section (magit-section) ())
(defclass yandex-arc/branch-section (magit-section) ())


(define-derived-mode yandex-arc-branches-mode magit-section-mode "arc-branches"
  "Yandex Arc Branches Major Mode."
  (setq-local revert-buffer-function 'yandex-arc/branches/revert-branches-buffer))


(defun yandex-arc/branches/show-all-branches ()
  (let ((buffer (get-buffer-create (concat "arc-branches: " (file-name-nondirectory default-directory)))))
    (set-buffer buffer)
    (yandex-arc-branches-mode)
    (yandex-arc/branches/revert-branches-buffer nil nil)
    (switch-to-buffer buffer)))


(defun yandex-arc/branches/revert-branches-buffer (ignore-auto noconfirm)
  (yandex-arc/branches/redraw-branches-buffer
   (append (oref (yandex-arc/shell/branch-list) :value) nil))) ; Append converts vector to list.


(defun yandex-arc/branches/redraw-branches-buffer (branch-infos)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (yandex-arc/root-section)
        (yandex-arc/branches/insert-branches-section
         "Local branches" nil
         (seq-filter (lambda (branch-info) (gethash "local" branch-info)) branch-infos))
        (yandex-arc/branches/insert-branches-section
         "Remote branches" t
         (seq-filter (lambda (branch-info) (not (gethash "local" branch-info))) branch-infos))))))


(defun yandex-arc/branches/insert-branches-section (section-name hide-section branch-infos)
  (magit-insert-section (yandex-arc/branches-section section-name hide-section)
    (magit-insert-heading
      (yandex-arc/properties/section-heading section-name)
      ":") ; Column at the end of the heading is replaced on subsections number.
    (dolist (branch-info branch-infos)
      (yandex-arc/branches/insert-branch-section
       (gethash "name" branch-info)
       (gethash "current" branch-info)))
    (insert ?\n)))


(defun yandex-arc/branches/insert-branch-section (name is-head)
  (magit-insert-section (yandex-arc/branch-section name)
    (magit-insert-heading
      (if is-head
          (concat
           (yandex-arc/properties/section-heading "@ ")
           (yandex-arc/properties/current-branch-name name))
        (concat
         "  "
         (yandex-arc/properties/branch-name name)))
      "\n")))
