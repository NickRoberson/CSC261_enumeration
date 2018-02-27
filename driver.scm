;; Lab: Propositional Logic
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Summary
;;   Provides a collection of tests for the (tt-entails? knowledge-base query),
;;   (tt-check-all knowledge-base query symbols model), and (pl-true? sentence
;;   model) functions.
;;
;; Provides
;;   (pl-true?-tests)
;;   (tt-entails?-tests)
;;   (tt-check-all-tests)
;;
;; Practica
;;   A sentence is either a boolean value, a symbol (representing a
;;   variable), or a list. When the sentence is a list, the first item
;;   must be one of the symbols 'not, 'and, 'or, '=>, or '<=>. If the
;;   symbol is 'not, the list must have only a second element, another
;;   sentence, otherwise the list must have two subsequent elements,
;;   sentences, that represent the arguments to the specified logical
;;   connectives.
;;
;;   More formally, a sentence is defined as follows:
;; 
;;     sentence := boolean | symbol | (not sentence) | 
;;                 (and sentence sentence) |
;;                 (or sentence sentence) |
;;                 (=> sentence sentence) |
;;                 (<=> sentence sentence)
;;
;;   A model is an association list whose keys (car of elements) are
;;   symbols and whose values (cdr of elements) are booleans.(require rackunit)

; load necessary libraries
(require rackunit)
(require rackunit/text-ui)
(load "logic.scm")
(load "enumeration.scm")

; instantiate data that tests rely on 
(define model
  (list (cons 'A #t)
        (cons 'B #f)
        (cons 'C #t)
        (cons 'D #f)))

; simpler example
(define knowledge-base-simple
  (list 'and
        (list 'and
              (list 'and (list '=> 'A 'B) (list '=> 'B 'C))
              (list '=> 'C 'D))
        'A))
(define kb-symbols-simple (get-symbols knowledge-base-simple))

; more complex example
(define knowledge-base-complex
  (list 'and 'A
        (list 'and
              (list '=> (list 'or 'A 'B) 'C)
              (list 'and
                    (list '=> (list 'or 'B 'C) 'A)
                    (list '<=> (list 'and 'B (list 'not 'D)) 'A)))))
(define kb-symbols-complex (get-symbols knowledge-base-complex))


; tests for (pl-true? sentence model)
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

; tests for (tt-entails? knowledge-base query)
(define tt-entails?-tests
  (test-suite
   "Tests for tt-entails?"
   (test-case
    "SIMPLE: { A, A => B, B => C, C => D }  B, C, D?"
    ; expect all to evaluate to true  
    (check-equal? (tt-entails? knowledge-base-simple 'B) #t)
    (check-equal? (tt-entails? knowledge-base-simple 'C) #t)
    (check-equal? (tt-entails? knowledge-base-simple 'D) #t))
   (test-case
    "COMPLEX: {A, A or B => C, B or C => A, B and not D <=> A } C, B, D?"
    ; expect B = #t, C = #t, D = #f
    (check-equal? (tt-entails? knowledge-base-complex 'C) #t)
    (check-equal? (tt-entails? knowledge-base-complex 'B) #t)
    (check-equal? (tt-entails? knowledge-base-complex 'D) #f))))



; tests for (tt-check-all knowledge-base query symbols model)
(define tt-check-all-tests
  (test-suite
   "Tests for tt-check-all"
   (test-case
    "SIMPLE: { A, A => B, B => C, C => D }  B, C, D?"
    ; expect all to evaluate to true  
    (check-equal? (tt-check-all knowledge-base-simple 'C kb-symbols-simple null) #t)
    (check-equal? (tt-check-all knowledge-base-simple 'B kb-symbols-simple null) #t)
    (check-equal? (tt-check-all knowledge-base-simple 'D kb-symbols-simple null) #t))
   (test-case
    "COMPLEX: {A, A or B => C, B or C => A, B and not D <=> A } C, B, D?"
    ; expect B = #t, C = #t, D = #f
    (check-equal? (tt-check-all knowledge-base-complex 'C kb-symbols-complex null) #t)
    (check-equal? (tt-check-all knowledge-base-complex 'B kb-symbols-complex null) #t)
    (check-equal? (tt-check-all knowledge-base-complex 'D kb-symbols-complex null) #f)
    )))


; run all tests
(display "Testing (pl-true? sentence model)...")(newline)
(run-tests pl-true?-tests)

(display "Testing (tt-entails? knowledge-base query)...")(newline)
(run-tests tt-entails?-tests)

(display "Testing (tt-check-all knowledge-base query symbols model)...")(newline)
(run-tests tt-check-all-tests)