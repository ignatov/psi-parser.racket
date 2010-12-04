#lang racket

(require rackunit
         rackunit/text-ui
         "../src/psi-parser.rkt"
         "../src/utils.rkt"
         )

(define input-dir "../input")

(define (check-file filename)
  (check-not-equal? (parse-file (read-text-file filename)) #f filename))

(define psi-parser-tests
  (test-suite
   "Tests for psi-parser.rkt"

   (for-each (lambda (f)
               (check-file (build-path (current-directory) input-dir f)))
             (directory-list input-dir))
   ))

(run-tests psi-parser-tests)