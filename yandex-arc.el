;; -*- lexical-binding: t; -*-

(require 'magit-section)

(defun yandex-arc ()
  "Starts Yandex Arc Major Mode in the current directory."
  (interactive)
  (yandex-arc/mode-init))


(define-derived-mode yandex-arc-mode magit-section-mode "arc"
  "Yandex Arc Major Mode."
  (setq revert-buffer-function
        (lambda (ignore-auto noconfirm) (yandex-arc/update-arc-buffer))))


(defun yandex-arc/mode-init ()
  "Initializes Yandex Arc Major Mode"
  (yandex-arc/update-root)
  (switch-to-buffer
   (concat "Arc: " (file-name-nondirectory yandex-arc/root)))
  (yandex-arc-mode)
  (yandex-arc/update-arc-buffer))


(defun yandex-arc/update-arc-buffer ()
  (yandex-arc/update-status)
  (yandex-arc/update-info)
  (yandex-arc/redraw-arc-buffer))


(defun yandex-arc/redraw-arc-buffer ()
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (yandex-arc/root-section)
        (magit-insert-section-body
          (yandex-arc/print-head-info)
          (yandex-arc/insert-status-section))))))


(defun yandex-arc/print-head-info ()
  "Adds information about HEAD to the buffer."
  (insert (yandex-arc/info-to-str))
  (insert ?\n))


(defun yandex-arc/info-to-str ()
  "Converts `yandex-arc/info-hash' to a string."
  (let ((branch  (gethash "branch"  yandex-arc/info-hash))
        (summary (gethash "summary" yandex-arc/info-hash)))
    (concat "Head: [" branch "] " summary)))


(defun yandex-arc/insert-status-section ()
  "Inserts a section with information from `yandex-arc/status-hash'."
  (let ((unstaged (yandex-arc/get-changed-paths "changed"))
        (staged   (yandex-arc/get-changed-paths  "staged")))
    (if (> (length unstaged) 0)
        (progn
          (insert ?\n)
          (magit-insert-section (yandex-arc/files-section)
            (magit-insert-heading (format "Unstaged changes (%d)" (length unstaged)))
            (magit-insert-section-body
              (insert (string-join unstaged ?\n) ?\n)))))
    (if (> (length staged) 0)
        (progn
          (insert ?\n)
          (magit-insert-section (yandex-arc/files-section)
            (magit-insert-heading (format "Staged changes (%d)" (length staged)))
            (magit-insert-section-body
              (insert (string-join staged ?\n) ?\n)))))))


(defun yandex-arc/get-changed-paths (location)
  "Extracts array of (un)staged changed paths `yandex-arc/status-hash'.

LOCATION can be \"changed\" or \"staged\""
  (seq-map
   (lambda (file-description) (gethash "path" file-description))
   (gethash location (gethash "status" yandex-arc/status-hash))))
