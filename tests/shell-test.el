;; -*- lexical-binding: t; -*-

(require 'yandex-arc-shell)

(require 'cl-lib)
(require 'ert)


(cl-defmacro yandex-arc/shell-test/with-process-output ((text return-code args-var) &body body)
  "Evaluate BODY with a stub for `process-file'.
The stub inserts TEXT into the current buffer and returns RETURN-CODE.
Bind ARGS-VAR to the latest call's command arguments,
excluding PROGRAM, INFILE, DESTINATION and DISPLAY."
  (declare (indent 1) (debug ((form form symbolp) body)))
  (let ((text-symbol (make-symbol "text"))
        (return-code-symbol (make-symbol "return-code"))
        (process-args-symbol (make-symbol "process-args")))
    `(let ((,text-symbol ,text)
           (,return-code-symbol ,return-code)
           (,args-var nil))
       (cl-letf (((symbol-function 'process-file)
                  (lambda (&rest ,process-args-symbol)
                    (setq ,args-var (nthcdr 4 ,process-args-symbol))
                    (insert ,text-symbol)
                    ,return-code-symbol)))
         ,@body))))


;; run-arc
(ert-deftest yandex-arc/shell-test/should-return-utf8-process-output-unchanged ()
  ;; Include supplementary characters, a ZWJ sequence, a variation
  ;; selector, a flag and a decomposed accented letter.
  (let* ((text " \tПривет 📄 😀 👩🏽‍💻 ⚙️ 🇯🇵 e\u0301\r\n\33[31m  \n")
         (yandex-arc/shell/arc-bin "printf")
         ;; Exercise UTF-8 decoding and CRLF preservation with a real process.
         (result (yandex-arc/shell/run-arc
                  "%s" (encode-coding-string text 'utf-8-unix))))
    (should (zerop (slot-value result 'return-code)))
    (should (equal (slot-value result 'value) text))))


(ert-deftest yandex-arc/shell-test/should-not-modify-calling-buffer ()
  (with-temp-buffer
    (insert "Caller text")
    (yandex-arc/shell-test/with-process-output ("Command output" 0 command-args)
      (let ((result (yandex-arc/shell/run-arc "test-command" "test-arg")))
        (should (zerop (slot-value result 'return-code))))
      (should (equal command-args '("test-command" "test-arg"))))
    (should (equal (buffer-string) "Caller text"))))


;; run-arc-json
(ert-deftest yandex-arc/shell-test/should-preserve-json-command-error-diagnostic ()
  (let ((text "Arc failed before producing JSON\r\n"))
    (yandex-arc/shell-test/with-process-output (text 1 command-args)
      (let ((result (yandex-arc/shell/run-arc-json "test-command" "test-arg")))
        (should (= (slot-value result 'return-code) 1))
        (should (equal (slot-value result 'value) text))
        (should (equal command-args '("test-command" "test-arg" "--json")))))))


(ert-deftest yandex-arc/shell-test/should-parse-json-command-output ()
  (let ((json "{\"branch\":\"test-branch\",\"paths\":[\"test-dir/test-file\"]}\r\n"))
    (yandex-arc/shell-test/with-process-output (json 0 command-args)
      (let ((result (yandex-arc/shell/run-arc-json "test-command" "test-arg")))
        (should (zerop (slot-value result 'return-code)))
        (should (equal (gethash "branch" (slot-value result 'value)) "test-branch"))
        (should (equal (gethash "paths" (slot-value result 'value)) ["test-dir/test-file"]))
        (should (equal command-args '("test-command" "test-arg" "--json")))))))


;; run-arc-text
(ert-deftest yandex-arc/shell-test/should-normalize-decoded-text-command-output ()
  (let ((text " \33[31mHello\33[0m\rnext line \n"))
    (yandex-arc/shell-test/with-process-output (text 0 command-args)
      (let ((result (yandex-arc/shell/run-arc-text "test-command" "test-arg")))
        (should (zerop (slot-value result 'return-code)))
        (should (equal (slot-value result 'value) "Hello\nnext line"))
        (should (equal command-args '("test-command" "test-arg")))))))
