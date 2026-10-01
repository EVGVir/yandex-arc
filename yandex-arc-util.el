;; -*- lexical-binding: t; -*-

(provide 'yandex-arc-util)

(require 'yandex-arc-properties)

(require 'ansi-color)
(require 'subr-x)


(defmacro yandex-arc/util/save-line-and-column (body)
  (let ((line   (gensym "line"))
        (column (gensym "column")))
    `(let ((,line   (line-number-at-pos))
           (,column (current-column)))
       ,body
       (goto-char (point-min))
       (forward-line   (1- ,line))
       (move-to-column ,column))))


(defun yandex-arc/util/read-branch-from-minibuffer (prompt &optional default-branch)
  "Reads branch from minibuffer, prompting with string PROMPT.

Branch under the point is proposed as the default value. If there
is no branch under the point DEFAULT-BRANCH is used."
  (read-from-minibuffer
   prompt
   (or
    (yandex-arc/properties/get-branch-name-at-point)
    default-branch)))


(defun yandex-arc/util/normalize-string (str)
  "Trim STR, remove ANSI sequences and replace CR characters with LF."
  (replace-regexp-in-string
   "\r" "\n"
   (ansi-color-filter-apply (string-trim str))))
