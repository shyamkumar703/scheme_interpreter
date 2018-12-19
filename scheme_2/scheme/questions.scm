(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (cons (map (lambda (pair) (car pair)) pairs) (cons (map (lambda (pair) (car (cdr pair))) pairs) nil))


)

(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define i 0)
  (define (help i s)
    (cond
      ((null? s) ())
      (else (cons (list i (car s)) (help (+ i 1) (cdr s))))
    )
  )
  (help 0 s)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS

(define (cons-all first rest)
  (define (func1 lst)
    (cons first lst)
  )
  (map func1 rest)
)

(define (list-change total denoms)
  (cond
    ((null? denoms) nil)
    ((= total 0) '(()))
    ((> (car denoms) total) (list-change total (cdr denoms)))
    (else
      (define diff (- total (car denoms)))
      (define first-elem (list-change diff denoms))
      (define without-first (list-change total (cdr denoms)))
      (define list1 (cons-all (car denoms) first-elem))
      (append list1 without-first)
    )
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons (cons 'lambda (cons (car (zip values) ) (map let-to-lambda body))) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
