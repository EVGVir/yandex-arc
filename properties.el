;; -*- lexical-binding: t; -*-


(defun yandex-arc/properties/branch-name (branch-name)
  (propertize branch-name
              'font-lock-face 'magit-branch-local
              'yandex-arc/properties/type-property 'branch-name
              'yandex-arc/properties/branch-name-property branch-name))


(defun yandex-arc/properties/current-branch-name (branch-name)
  (propertize branch-name
              'font-lock-face 'magit-branch-current
              'yandex-arc/properties/type-property 'branch-name
              'yandex-arc/properties/branch-name-property branch-name))


(defun yandex-arc/properties/get-branch-name-at-point ()
  (get-text-property (point) 'yandex-arc/properties/branch-name-property))


(defun yandex-arc/properties/section-heading (section-name)
  (propertize section-name 'font-lock-face 'magit-section-heading))


(defun yandex-arc/properties/diff-file-heading (file-name)
  (propertize file-name
              'font-lock-face 'magit-diff-file-heading
              'yandex-arc/properties/type-property 'file-name
              'yandex-arc/properties/file-name-property file-name))


(defun yandex-arc/properties/file-name (file-name)
  (propertize file-name
              'font-lock-face 'magit-filename
              'yandex-arc/properties/type-property 'file-name
              'yandex-arc/properties/file-name-property file-name))


(defun yandex-arc/properties/hash (hash)
  (propertize hash
              'font-lock-face 'magit-hash
              'yandex-arc/properties/type-property 'hash
              'yandex-arc/properties/hash-property hash))


(defun yandex-arc/properties/link (name href-format &rest args)
  (let ((href (apply 'format (append (list href-format) args))))
    (propertize (button-buttonize name 'browse-url href)
                'yandex-arc/properties/type-property 'link)))
