#lang racket

(require (planet bzlib/parseq:1:3))

;; identifier := [a-zA-Z][a-zA-Z0-9_]*
(define identifier
  (seq c <- alpha
       rest <- (zero-many word)
       (return (string->symbol
                (list->string (cons c rest))))))

;; op := "+" | "-" | "*" | "/" | "==" | "<" | ">" | "<>" | "<=" | ">="
(define op
  (choice
   (tokens "+" (return '+))
   (tokens "-" (return '-))
   (tokens "*" (return '*))
   (tokens "/" (return '/))
   (tokens "<" (return '>))
   (tokens ">" (return '>))
   (tokens "==" (return '==))
   (tokens "<>" (return '<>))
   (tokens "<=" (return '<=))
   (tokens ">=" (return '>=))
   ))

;; expr := term op term
(define expr (tokens lhs <- term
                     (let loop ((lhs lhs))
                       (choice (tokens opr <- op
                                       rhs <- term
                                       (loop (list opr lhs rhs)))
                               (return lhs)))))
;; term := factor op factor
(define term (tokens lhs <- factor
                     (let loop ((lhs lhs))
                       (choice (tokens opr <- op
                                       rhs <- factor
                                       (loop (list opr lhs rhs)))
                               (return lhs)))))

;; factor := number | '(' exp ')'
(define factor (choice real-number occurence (bracket #\( expr #\))))

;; package := 'P' name '{' relation * ';' '}'
(define package
  (tokens "P"
          name <- identifier
          r <- (bracket/delimited #\{ relation #\; #\})
          (return `(package, name, r))))

;; relation := scheme | task
(define relation
  (choice scheme task))

;; scheme := 'S' name | '{' defs '|' fls '}'
(define scheme
  (tokens "S"
          name <- identifier
          #\{
          defs <- definitions-scope
          #\|
          fls <- functional-links-scope
          #\}
          variants <- (zero-one if-statement `())
          (return `(scheme, name, defs, fls, variants))))

;; variant-part := '{' defs '|' fls '}' | fls
(define variant-part
  (choice
   (tokens #\{
           defs <- definitions-scope
           #\|
           fls <- functional-links-scope
           #\}
           (return `(variant-part, defs, fls)))
   (tokens fls <- functional-links-scope
           (return `(variant-part, `(), fls)))))

(define if-statement
  (tokens "if"
          guard <- expr
          "then"
          positive <- variant-part
          "else"
          negative <- variant-part
          "fi"
          (return `(if-statement, guard, positive, negative))))

;; task := 'Q' name '{' 'on' scheme-name 'in' in 'out' out }
(define task
  (tokens "Q"
          name <- identifier
          #\{
          "on" scheme-name <- identifier
          "in"  in  <- (delimited occurence #\,)
          "out" out <- (delimited occurence #\,)
          #\}
          (return `(task, name, scheme-name, in, out))))

;; occurence := identifier | identifier.identifier
(define occurence
  (choice
   (tokens name <- identifier
           #\.
           surname <- identifier
           (return `(c, name, surname)))
   (tokens name <- identifier
           (return `(c, name, null)))))

;; type := "bool" | "nat" | "int" | "string" | "real"
(define type
  (choice (tokens "bool" (return 'bool))
          (tokens "nat" (return 'nat))
          (tokens "int" (return 'int))
          (tokens "string" (return 'string))
          (tokens "real" (return 'real))
          ))

;; definitions := type name * ','
(define definitions
  (tokens t <- type
          names <- (delimited identifier #\,)
          (return (map (lambda (name) `(definition, t, name))
                       names))))

;; definition-scope := definitions * ';'
(define definitions-scope
  (tokens defs <- (delimited definitions #\;)
          (return (foldr append `() defs)
                  )))

;; functional-link := name '<-' expr
(define functional-link
  (tokens name <- identifier
          "<-"
          value <- expr
          (return `(name, value))))

;; functional-links-scope := functional-link * ';'
(define functional-links-scope
  (delimited functional-link #\;))

(define read-package (make-reader package))

(define (parse-file in)
  (read-package
   (regexp-replace* #rx"(?m://.*)" in "")))

(provide parse-file)