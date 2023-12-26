;; -*- lexical-binding: t; -*-

(defmacro yandex-arc/util/save-line-and-column (body)
  (let ((line   (gensym "line"))
        (column (gensym "column")))
    `(let ((,line   (line-number-at-pos))
           (,column (current-column)))
       ,body
       (goto-char (point-min))
       (forward-line   (1- ,line))
       (move-to-column ,column))))
