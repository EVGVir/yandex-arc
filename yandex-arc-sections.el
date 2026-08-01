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
