(require rackunit)
(require rackunit/text-ui)
(load "logic.scm")
(load "enumeration.scm")

(define model (list (cons 'A #t)
                    (cons 'B #f)
                    (cons 'C #t)
                    (cons 'D #f)))

(define pl-true?-tests
  (test-suite
   "Tests for pl-true?"
   (test-case
    "boolean #t"
    (check-equal? (pl-true? #t model) #t ))
   (test-case
    "boolean #f" 
    (check-equal? (pl-true? #f model) #f))
   (test-case
    "symbol #t"
    (check-equal? (pl-true? 'A model) #t))
   (test-case
    "symbol #f"
    (check-equal? (pl-true? 'B model) #f))
   (test-case
    "negation #t"
    (check-equal? (pl-true? (list 'not 'B) model) #t))
   (test-case
    "negation #f"
    (check-equal? (pl-true? (list 'not 'A) model) #f))
   (test-case
    "conjunction #t"
    (check-equal? (pl-true? (list 'and 'A 'C) model) #t))
   (test-case
    "conjunction #f"
    (check-equal? (pl-true? (list 'and 'B 'A) model) #f))
   (test-case
    "disjunction #t"
    (check-equal? (pl-true? (list 'or 'A 'B) model) #t))
   (test-case
    "disjunction #f"
    (check-equal? (pl-true? (list 'or 'B 'D) model) #f))
   (test-case
    "implication #t => #t"
    (check-equal? (pl-true? (list '=> 'A 'C) model) #t))
   (test-case
    "implication #t => #f"
    (check-equal? (pl-true? (list '=> 'A 'B) model) #f))
   (test-case
    "implication #f => #t"
    (check-equal? (pl-true? (list '=> 'B 'A) model) #t))
   (test-case
    "bidirectional implication #t <=> #t"
    (check-equal? (pl-true? (list '<=> 'A 'C) model) #t))
   (test-case
    "bidirectional implication #t <=> #f"
    (check-equal? (pl-true? (list '<=> 'A 'B) model) #f))
   (test-case
    "bidirectional implication #f <=> #f"
    (check-equal? (pl-true? (list '<=> 'B 'D) model) #t))))


(run-tests pl-true?-tests)