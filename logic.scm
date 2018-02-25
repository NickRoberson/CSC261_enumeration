;;
;; File
;;   logic.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Support routines dealing with the symbols in propositional sentences
;;
;; Provides
;;   (get-symbols sentence)
;;   (list-union list1 list2)
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
;;   symbols and whose values (cdr of elements) are booleans.

;;------------------------------------------------------------------------------

;;
;; Procedure
;;   get-symbols
;;
;; Purpose
;;   Extract symbols from a sentence
;;
;; Parameters
;;   sentence, a sentence
;;
;; Produces
;;   symbols, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   Each element of symbols is used in sentence. 
;;   symbols contains no element that is not used in sentence.
;;   All symbols from sentence appear in symbols.
;;   symbols contains no repeated values.
(define get-symbols
  (lambda (sentence)
    (cond
     ;; Base cases
     ;;   Simple value
     ((boolean? sentence) '())
     ;;   Ground term
     ((symbol? sentence) (list sentence))
     (else
      ;; Recursive cases
      (let ((op (car sentence))     ;; Extract operator
	    (arg1 (cadr sentence))) ;; First argument
	(if (equal? 'not op)   ;; not statement?
	    (get-symbols arg1) ;; get-symbols from not argument
	    ;; remaining possibilities all take two arguments
	    (let ((arg2 (caddr sentence))) ;; Second argument
	      (list-union (get-symbols arg1) ;; Merge symbols in arg1 and
		     (get-symbols arg2))))))))) ;; arg2

;;
;; Procedure
;;   list-union
;;
;; Purpose
;;   Compute the union of the elements in two lists
;;
;; Parameters
;;   list1, a list
;;   list2, a list
;;
;; Produces
;;   union, a list
;;
;; Preconditions
;;   [No additional]
;;
;; Postconditions
;;   For all 0 <= i < (length list1)
;;     (member (list-ref list1 i) union)
;;   For all 0 <= i < (length list2)
;;     (member (list-ref? list2 i) union)
;;    For all 0 <= k,m < (length union) where k not equal to m
;;      (not (equal? (list-ref union k) (list-ref union m)))
(define list-union
  (lambda (list1 list2)
    (let ((set1 (remove-duplicates list1))
	  (set2 (remove-duplicates list2)))
      (remove-duplicates (append set1 set2)))))

;;
;; Procedure
;;   remove-duplicates
;;
;; Purpose
;;   Remove duplicate entries from a list
;;
;; Parameters
;;   vals, a list
;;
;; Produces
;;   single-vals, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   For all 0 <= i < (length vals)
;;     (member (list-ref vals i) single-vals)
;;   For all 0 <= i < (length single-vals)
;;     (member (list-ref single-vals i) vals)
;;    For all 0 <= k,m < (length single-vals) where k not equal to m
;;      (not (equal? (list-ref single-vals k) (list-ref single-vals m)))
(define remove-duplicates
  (lambda (vals)
    (let loop ((so-far '())
	       (remaining vals))
      (cond 
       ((null? remaining)               ; Nothing left
	so-far)                         ; return answer so far
       ((member (car remaining) so-far) ; First item already included
	(loop so-far (cdr remaining)))  ; Continue to remaining items
       (else
	(loop (cons (car remaining) so-far) ; Add first item and continue
	      (cdr remaining)))))))

