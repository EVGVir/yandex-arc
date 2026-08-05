;; -*- lexical-binding: t; -*-

(provide 'yandex-arc-sections)

(require 'eieio)
(require 'magit-section)


(defclass yandex-arc/sections/branch-section            (magit-section) ())
(defclass yandex-arc/sections/branches-section          (magit-section) ())
(defclass yandex-arc/sections/files-section             (magit-section) ())
(defclass yandex-arc/sections/revision-message-section  (magit-section) ())
(defclass yandex-arc/sections/revision-summary-section  (magit-section) ())
(defclass yandex-arc/sections/root-section              (magit-section) ())
(defclass yandex-arc/sections/stash-section             (magit-section) ())
(defclass yandex-arc/sections/stashes-section           (magit-section) ())


(defun yandex-arc/sections/get-file-names-at-point ()
  (let ((section (magit-current-section)))
    (cond ((magit-section-match 'magit-file-section section)
           (list (slot-value section 'value)))
          ((magit-section-match 'yandex-arc/sections/files-section section)
           (seq-map (lambda (section) (slot-value section 'value))
                    (slot-value section 'children))))))


(defun yandex-arc/sections/hunk-at-point-p ()
  (when-let* ((section (magit-current-section)))
    (magit-section-match 'magit-hunk-section section)))


(defun yandex-arc/sections/line-in-file (hunk-section pos)
  "Return the line number in the file for POS in HUNK-SECTION."
  (let ((content-start (slot-value hunk-section 'content)))
    (save-excursion
      (goto-char content-start)
      (let ((target (save-excursion (goto-char pos) (line-beginning-position)))
            (offset 0))
        ;; Count lines from hunk content start to cursor position,
        ;; skipping removed lines (starting with `-') since they
        ;; don't exist in the file after changes.
        (while (< (point) target)
           (unless (looking-at-p "-")
             (setq offset (1+ offset)))
           (forward-line 1))
        ;; Parse hunk header `@@ -X,Y +Z,W @@` to get Z (start line
        ;; in file after changes) and add the offset to it.
        (let ((header (string-trim
                       (buffer-substring-no-properties
                        (slot-value hunk-section 'start)
                        content-start))))
          (when (string-match "^@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\)" header)
            (+ (string-to-number (match-string 1 header)) offset)))))))
