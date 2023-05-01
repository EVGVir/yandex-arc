;; -*- lexical-binding: t; -*-

(require 'magit-section)

(defun yandex-arc ()
  "Starts Yandex Arc Major Mode in the current directory."
  (interactive)
  (yandex-arc/mode-init))


(defvar-keymap yandex-arc-mode-map
  :doc "Keymap for `yandex-arc-mode'."
  "RET" 'yandex-arc/visit-file
  "s"   'yandex-arc/stage-file
  "u"   'yandex-arc/unstage-file)


(define-derived-mode yandex-arc-mode magit-section-mode "arc"
  "Yandex Arc Major Mode."
  (setq revert-buffer-function
        (lambda (ignore-auto noconfirm) (yandex-arc/update-arc-buffer))))


(defun yandex-arc/mode-init ()
  "Initializes Yandex Arc Major Mode"
  (yandex-arc/shell/update-root)
  (switch-to-buffer
   (concat "Arc: " (file-name-nondirectory yandex-arc/shell/root)))
  (yandex-arc-mode)
  (yandex-arc/update-arc-buffer))


(defun yandex-arc/update-arc-buffer ()
  (yandex-arc/shell/update-status)
  (yandex-arc/shell/update-info)
  (yandex-arc/redraw-arc-buffer))


(defun yandex-arc/redraw-arc-buffer ()
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (yandex-arc/root-section)
        (yandex-arc/print-head-info)
        (yandex-arc/insert-status-section)))))


(defun yandex-arc/print-head-info ()
  "Adds information about HEAD to the buffer."
  (insert (yandex-arc/info-to-str))
  (insert ?\n))


(defun yandex-arc/info-to-str ()
  "Converts `yandex-arc/shell/info-hash' to a string."
  (let ((branch  (gethash "branch"  yandex-arc/shell/info-hash))
        (summary (gethash "summary" yandex-arc/shell/info-hash)))
    (concat "Head: [" branch "] " summary)))


(defun yandex-arc/insert-status-section ()
  "Inserts a section with information from `yandex-arc/shell/status-hash'."
  (let ((unstaged (yandex-arc/get-changed-paths "changed"))
        (staged   (yandex-arc/get-changed-paths  "staged")))
    (when (> (length unstaged) 0)
      (insert ?\n)
      (yandex-arc/insert-files-section
       (format "Unstaged changes (%d)" (length unstaged))
       unstaged :unstaged))
    (when (> (length staged) 0)
      (insert ?\n)
      (yandex-arc/insert-files-section
       (format "Staged changes (%d)" (length staged))
       staged :staged))))


(defun yandex-arc/insert-files-section (heading file-names diff-type)
  "Inserts a section with information about files FILE-NAMES. DIFF-TYPE controls
the diff type displayed for each file.

DIFF-TYPE one of:
* nil       - no diff should be displayed for files.
* :staged   - diff between HEAD and index must be displayed.
* :unstaged - diff between working tree and index must be displayed."
  (magit-insert-section (yandex-arc/files-section)
    (magit-insert-heading heading)
    (dolist (file-name file-names)
      (yandex-arc/insert-file-section
       file-name
       (yandex-arc/diff-file file-name diff-type)))))) ; TODO: Calculate diff lazy (pass lambda?)


(defun yandex-arc/insert-file-section (file-name diff)
  "Insert a section with a file."
  (magit-insert-section (yandex-arc/file-section file-name t)
    (magit-insert-heading file-name)
    (magit-insert-section-body
      (when diff
        (save-excursion (insert diff))
        (magit-wash-sequence 'yandex-arc/file-section-washer)))))


(defun yandex-arc/get-changed-paths (location)
  "Extracts array of (un)staged changed paths `yandex-arc/shell/status-hash'.

LOCATION can be \"changed\" or \"staged\""
  (seq-map
   (lambda (file-description) (gethash "path" file-description))
   (gethash location (gethash "status" yandex-arc/shell/status-hash))))


(defun yandex-arc/get-file-name-from-file-section ()
  (magit-section-value-if 'yandex-arc/file-section))


(defun yandex-arc/visit-file (file-name)
  "Visits file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (find-file file-name)))


(defun yandex-arc/stage-file (file-name)
  "Stages file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (yandex-arc/shell/stage file-name)
    (yandex-arc/update-arc-buffer)))


(defun yandex-arc/unstage-file (file-name)
  "Unstages file at point."
  (interactive (list (yandex-arc/get-file-name-from-file-section)))
  (when file-name
    (yandex-arc/shell/unstage file-name)
    (yandex-arc/update-arc-buffer)))


(defun yandex-arc/diff-file (file-name diff-type)
  (cond
   ((eq diff-type nil) nill)
   ((eq diff-type :staged)   (yandex-arc/shell/diff-staged   file-name))
   ((eq diff-type :unstaged) (yandex-arc/shell/diff-unstaged file-name))))


(defun yandex-arc/file-section-washer ()
  (unless (eobp)
    (delete-region (line-beginning-position) (1+ (line-end-position 2)))
    (let ((hunk-counter 0))
      (while (not (eobp))
        (when (not (looking-at-p "@@")) (error "Current line of the diff is not start of a hunk"))
        (magit-insert-section (yandex-arc/diff-hunk-section hunk-counter)
          (magit-wash-sequence 'yandex-arc/diff-hunk-section-washer))
        (setq hunk-counter (1+ hunk-counter))))))


(defun yandex-arc/diff-hunk-section-washer ()
  (forward-line)
  (magit-insert-heading)
  (forward-line)
  (while (and (not (eobp)) (not (looking-at-p "@@")))
    (forward-line)))
