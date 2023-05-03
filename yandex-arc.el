;; -*- lexical-binding: t; -*-

(require 'magit-section)

(defun yandex-arc ()
  "Starts Yandex Arc Major Mode in the current directory."
  (interactive)
  (yandex-arc/mode-init))


(defvar-keymap yandex-arc-mode-map
  :doc "Keymap for `yandex-arc-mode'."
  "RET" 'yandex-arc/actions/visit-file
  "s"   'yandex-arc/actions/stage-file
  "u"   'yandex-arc/actions/unstage-file)


(define-derived-mode yandex-arc-mode magit-section-mode "arc"
  "Yandex Arc Major Mode."
  (setq revert-buffer-function
        (lambda (ignore-auto noconfirm) (yandex-arc/update-arc-buffer))))


(defclass yandex-arc/root-section      (magit-section) ())
(defclass yandex-arc/files-section     (magit-section) ())
(defclass yandex-arc/file-section      (magit-section) ())
(defclass yandex-arc/diff-hunk-section (magit-section) ())


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
    (concat "Head:     "
            (propertize branch 'font-lock-face 'magit-branch-local)
            " " summary)))


(defun yandex-arc/insert-status-section ()
  "Inserts a section with information from `yandex-arc/shell/status-hash'."
  (let ((unstaged (yandex-arc/get-changed-paths "changed"))
        (staged   (yandex-arc/get-changed-paths  "staged")))
    (when (> (length unstaged) 0)
      (insert ?\n)
      (yandex-arc/insert-files-section "Unstaged changes" unstaged :unstaged))
    (when (> (length staged) 0)
      (insert ?\n)
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
      (yandex-arc/insert-file-section file-name diff-type))))


(defun yandex-arc/insert-file-section (file-name diff-type)
  "Insert a section with a file."
  (magit-insert-section (yandex-arc/file-section file-name t)
    (magit-insert-heading
      (propertize file-name 'font-lock-face 'magit-diff-file-heading))
    (magit-insert-section-body
      (when diff-type
        (yandex-arc/insert-diff-hunk-sections
         (yandex-arc/split-diff (yandex-arc/shell/diff-file file-name diff-type)))))))


(defun yandex-arc/insert-diff-hunk-sections (hunks)
  (dolist (hunk hunks)
    (magit-insert-section (yandex-arc/diff-hunk-section)
      (let ((header-end (1+ (string-match "\n" hunk))))
        (magit-insert-heading
          (propertize (substring hunk 0 header-end) 'font-lock-face 'magit-diff-hunk-heading))
        (insert (substring hunk header-end nil))))))


(defun yandex-arc/get-changed-paths (location)
  "Extracts array of (un)staged changed paths `yandex-arc/shell/status-hash'.

LOCATION can be \"changed\" or \"staged\""
  (seq-map
   (lambda (file-description) (gethash "path" file-description))
   (gethash location (gethash "status" yandex-arc/shell/status-hash))))


(defun yandex-arc/get-file-name-from-file-section ()
  (magit-section-value-if 'yandex-arc/file-section))


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
