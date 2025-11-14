;;; Test runner script for die-trying
(format t "~%Loading Quicklisp...~%")
(load "~/quicklisp/setup.lisp")

(format t "~%Loading die-trying test system...~%")
(push #P"/app/" asdf:*central-registry*)
;; Load the system definition and load dependencies via Quicklisp
(asdf:load-asd (merge-pathnames "die-trying.asd" #P"/app/"))
(ql:quickload :fiveam :silent t)
(ql:quickload :die-trying :silent t)
;; Now load the test system via ASDF
(asdf:load-system :die-trying-tests)

(format t "~%Running tests...~%")
(format t "================================~%")

(let ((results (fiveam:run 'die-trying-tests::die-trying-tests)))
  (format t "~%================================~%")
  (format t "Test Results Summary:~%")
  (format t "  Total tests: ~a~%" (length results))
  (format t "  Passed: ~a~%" (count-if (lambda (r) (typep r 'fiveam::test-passed)) results))
  (format t "  Failed: ~a~%" (count-if (lambda (r) (typep r 'fiveam::test-failure)) results))
  (format t "  Errors: ~a~%" (count-if (lambda (r) (typep r 'fiveam::unexpected-test-failure)) results))
  (format t "================================~%")
  
  ;; Exit with appropriate code
  (if (every (lambda (r) (typep r 'fiveam::test-passed)) results)
      (progn
        (format t "~%All tests passed!~%")
        (quit :unix-status 0))
      (progn
        (format t "~%Some tests failed!~%")
        (quit :unix-status 1))))

