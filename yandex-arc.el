;; -*- lexical-binding: t; -*-

(require 'magit)
(require 'magit-section)
(require 'transient)


(defun yandex-arc ()
  "Starts Yandex Arc Major Mode in the current directory."
  (interactive)
  (yandex-arc/mode-init))


(defvar-keymap yandex-arc-mode-map
  :doc "Keymap for `yandex-arc-mode'."
  "RET" 'yandex-arc/actions/visit-file
  "s"   'yandex-arc/actions/stage-file
  "u"   'yandex-arc/actions/unstage-file
  "z"   'yandex-arc/actions/stash-transient)


(define-derived-mode yandex-arc-mode magit-section-mode "arc"
  "Yandex Arc Major Mode."
  (setq revert-buffer-function
        (lambda (ignore-auto noconfirm) (yandex-arc/update-arc-buffer))))


(defclass yandex-arc/root-section    (magit-section) ())
(defclass yandex-arc/files-section   (magit-section) ())
(defclass yandex-arc/stashes-section (magit-section) ())
(defclass yandex-arc/stash-section   (magit-section) ())


(defun yandex-arc/mode-init ()
  "Initializes Yandex Arc Major Mode"
  (yandex-arc/shell/update-root)
  (switch-to-buffer
   (concat "Arc: " (file-name-nondirectory yandex-arc/shell/root)))
  (yandex-arc-mode)
  (yandex-arc/update-arc-buffer))


(defun yandex-arc/update-arc-buffer ()
  (yandex-arc/redraw-arc-buffer
   (yandex-arc/shell/info)
   (yandex-arc/shell/status)
   (yandex-arc/shell/stash-list)))


(defun yandex-arc/redraw-arc-buffer (info status stash-info)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (yandex-arc/root-section)
        (yandex-arc/print-head-info info)
        (yandex-arc/insert-status-section status)
        (yandex-arc/insert-stashes-section (yandex-arc/shell/stash-list))))))


(defun yandex-arc/print-head-info (info)
  "Adds information about HEAD to the buffer."
  (let ((branch  (gethash "branch"  info))
        (summary (gethash "summary" info)))
    (insert "Head:     "
            (propertize branch 'font-lock-face 'magit-branch-local)
            " " summary "\n\n")))


(defun yandex-arc/insert-status-section (status)
  "Inserts a section with information about staged and unstaged
files taken from STATUS."
  (let ((untracked (yandex-arc/get-changed-paths status "untracked"))
        (unstaged  (yandex-arc/get-changed-paths status "changed"))
        (staged    (yandex-arc/get-changed-paths status "staged")))
    (when (> (length untracked) 0)
      (yandex-arc/insert-files-section "Untracked files" untracked nil))
    (when (> (length unstaged) 0)
      (yandex-arc/insert-files-section "Unstaged changes" unstaged :unstaged))
    (when (> (length staged) 0)
      (yandex-arc/insert-files-section "Staged changes" staged :staged))))


(defun yandex-arc/insert-files-section (heading file-names diff-type)
  "Inserts a section with information about files FILE-NAMES. If
DIFF-TYPE is not nil then diff is displayed for each
file. Possible values of DIFF-TYPE are described in
`yandex-arc/shell/diff-file'."
  (magit-insert-section (yandex-arc/files-section)
    (magit-insert-heading
      (propertize heading 'font-lock-face 'magit-section-heading) " "
      (propertize (concat "(" (number-to-string (length file-names)) ")")
                  'font-lock-face 'magit-section-child-count))
    (dolist (file-name file-names)
      (yandex-arc/insert-file-section file-name diff-type))
    (insert ?\n)))


(defun yandex-arc/insert-file-section (file-name diff-type)
  "Insert a section with a file."
  (magit-insert-section (magit-file-section file-name t)
    (if diff-type
        (progn
          (magit-insert-heading
            (propertize file-name 'font-lock-face 'magit-diff-file-heading))
          (magit-insert-section-body
            (yandex-arc/insert-diff-hunk-sections
             (yandex-arc/split-diff (yandex-arc/shell/diff-file file-name diff-type))))
          )
      (insert (propertize file-name 'font-lock-face 'magit-filename) ?\n))))


(defun yandex-arc/insert-diff-hunk-sections (hunks)
  (dolist (hunk hunks)
    (magit-insert-section (magit-hunk-section)
      (let ((header-end (1+ (string-match "\n" hunk))))
        (magit-insert-heading (substring hunk 0 header-end))
        (insert (substring hunk header-end nil))))))


(defun yandex-arc/get-changed-paths (status location)
  "Extracts array of (un)staged changed paths from STATUS.

LOCATION can be \"changed\", \"staged\" or \"untracked\"."
  (seq-map
   (lambda (file-description) (gethash "path" file-description))
   (gethash location (gethash "status" status))))


(defun yandex-arc/get-file-name-from-file-section ()
  (magit-section-value-if 'magit-file-section))


(defun yandex-arc/split-diff (diff)
  "Splits DIFF into hunks."
  (with-temp-buffer
    (insert diff)
    (goto-char 0)
    (re-search-forward "^@@")
    (let (result
          (begin (line-beginning-position)))
      (while (re-search-forward "^@@" nil t)
        (setq result (append result (list (buffer-substring begin (line-beginning-position)))))
        (setq begin (line-beginning-position)))
      (setq result (append result (list (buffer-substring begin (buffer-end 1))))))))


(defun yandex-arc/insert-stashes-section (stash-info)
  (let ((stashes-num (length stash-info)))
    (when (> stashes-num 0)
      (insert ?\n)
      (magit-insert-section (yandex-arc/stashes-section nil t)
        (magit-insert-heading
          (propertize "Stashes" 'font-lock-face 'magit-section-heading) " "
          (propertize (concat "(" (number-to-string stashes-num) ")")
                      'font-lock-face 'magit-section-child-count))
        (dotimes (stash-ix stashes-num)
          (yandex-arc/insert-stash-section
           stash-ix
           (gethash "description" (elt stash-info stash-ix))))
        (insert ?\n)))))


(defun yandex-arc/insert-stash-section (index description)
  (magit-insert-section (yandex-arc/stash-section index)
    (magit-insert-heading
       (propertize (format "stash@{%d}" index) 'font-lock-face 'magit-hash)
       " " description "\n")))

