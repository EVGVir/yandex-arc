;; -*- lexical-binding: t; -*-

(require 'yandex-arc-actions)

(require 'ert)
(require 'cl-lib)


(ert-deftest yandex-arc/actions-test/should-report-stage-failure-and-refresh ()
  (let (calls (refreshes 0))
    (cl-letf (((symbol-function 'yandex-arc/sections/get-file-names-at-point)
               (lambda () '("first" "failed" "untouched")))
              ((symbol-function 'yandex-arc/shell/stage)
               (lambda (path)
                 (push path calls)
                 (yandex-arc/arc-result
                  :return-code (if (equal path "failed") 1 0)
                  :value "Arc staging failure")))
              ((symbol-function 'revert-buffer) (lambda (&rest _) (cl-incf refreshes))))
      (let ((failure (should-error (yandex-arc/actions/stage-file) :type 'user-error)))
        (should (string-match-p "Unable to stage failed: Arc staging failure"
                                 (error-message-string failure))))
      (should (equal (nreverse calls) '("first" "failed")))
      (should (= refreshes 1)))))


(ert-deftest yandex-arc/actions-test/should-report-unstage-failure-and-refresh ()
  (let (calls (refreshes 0))
    (cl-letf (((symbol-function 'yandex-arc/sections/get-file-names-at-point)
               (lambda () '("first" "failed" "untouched")))
              ((symbol-function 'yandex-arc/shell/unstage)
               (lambda (path)
                 (push path calls)
                 (yandex-arc/arc-result
                  :return-code (if (equal path "failed") 1 0)
                  :value "Arc unstaging failure")))
              ((symbol-function 'revert-buffer) (lambda (&rest _) (cl-incf refreshes))))
      (let ((failure (should-error (yandex-arc/actions/unstage-file) :type 'user-error)))
        (should (string-match-p "Unable to unstage failed: Arc unstaging failure"
                                 (error-message-string failure))))
      (should (equal (nreverse calls) '("first" "failed")))
      (should (= refreshes 1)))))
