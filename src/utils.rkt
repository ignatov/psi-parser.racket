#lang racket

(define (read-text-file file-name)
  (let* ((ip (open-input-file file-name))
         (res (read-text-file-from-input-port ip)))
    (close-input-port ip)
    res))

(define file-chunck 1000)

(define (read-text-file-from-input-port input-port)
  (let* ((res (read-text-file-portion input-port file-chunck)))
    (if (not (eof-object? (peek-char input-port)))
        (string-append res (read-text-file-from-input-port input-port))
        res)))

(define (read-text-file-portion input-port portion)
  ; read up to portion characters from input-port
  (let ((str (make-string portion #\space)))
    (read-into-string input-port str 0 portion)))

(define (read-into-string input-port str position max)
  ; read up to max characters into str in a tail recursive fashion.
  ; return the string
  (if (= position max)
      str
      (let ((ch (read-char input-port)))
        (cond ((eof-object? ch) (substring str 0 position))
              (else (begin
                      (string-set! str position ch)
                      (read-into-string input-port str (+ position 1) max)))))))

(provide read-text-file)