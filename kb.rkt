(load "logic.scm")
(load "enumeration.scm")
;(require "weinman-enumeration.scm")

; unicorn-kb
; 
; If the unicorn is mythical, then it is immortal, but if it is not mythical,
; then it is a mortal mammal. If the unicorn is either immortal or a mammal,
; then it is horned. The unicorn is magical if it is horned.

; given that we know a unicorn is horned we say that we know 'Horned and in
; our model we can set that to be true.

(define unicorn-kb
  ; if mythical then immortal
  (list 'and (list '=> 'Mythical 'Immortal)
        ; if not mythical then mortal
        (list 'and (list '=> (list 'not 'Mythical) 'Mortal)
              ; if immortal or mammal then horned
              (list 'and (list '=> (list 'or 'Immortal 'Mammal) 'Horned)
                    ; if horned then magical 
                    (list '=> 'Horned 'Magical)))))

(tt-entails? unicorn-kb 'Mythical)
(tt-entails? unicorn-kb 'Magical)
(tt-entails? unicorn-kb 'Horned)

; portia-kb
;
; In Shakespeare’s Merchant of Venice Portia had three caskets—gold, silver,
; and lead—inside one of which was Portia’ s portrait. The suitor was to
; choose one of the caskets, and if he was lucky enough (or wise enough)
; to choose the one with the portrait, then he could claim Portia as his bride.
; On the lid of each casket was an inscription to help the suitor choose wisely.
; Now, suppose Portia wished to choose her husband not on the basis of virtue,
; but simply on the basis of intelligence. She had the following inscriptions
; put on the caskets.
;      Gold: The portrait is in this casket
;      Silver: The portrait is not in this casket
;      Lead: The portrait is not in the gold casket
; 
; Portia explained to the suitor that of the three statements, at most one
; was true

(define portia-kb
  (list 'and
        ; at most one of these statements 'Gold, 'Silver, 'Lead is true
        ; 4 cases, one is true (3) or none are true (1). XOR clause
        ; limiting to at most one of these is true. 
        (list 'or
              (list 'and (list 'not 'Gold)
                    (list 'and (list 'not 'Silver)
                          (list 'not 'Lead)))
              (list 'or (list 'and 'Gold
                              (list 'and (list 'not 'Silver)
                                    (list 'not 'Lead)))
                    (list 'or (list 'and (list 'not 'Gold)
                                    (list 'and 'Silver
                                          (list 'not 'Lead)))
                          (list 'and (list 'not 'Gold)
                                (list 'and (list 'not 'Silver)
                                      'Lead)))))
        ; gold statement: if gold is true then it is in there
        (list 'and (list '=> 'Gold 'Gold)
              ; silver statement: if silver is true then the answer is gold
              ; or lead 
              (list 'and (list '=> 'Silver (list 'or 'Gold 'Lead))
                    ; lead statement: if lead is true then it is not in gold 
                    (list '=> 'Lead (list 'not 'Gold))))))

(tt-entails? portia-kb 'Gold)
(tt-entails? portia-kb 'Silver)
(tt-entails? portia-kb 'Lead)
; liars-kb

; There are many situations in life in which it is good to have one’s wits
; about one. So I shall now give you detailed, step-by-step instructions
; which will teach you ... how to avoid werewolves in the forest. Of course,
; I cannot absolutely guarantee that you will actually meet with any of these
; situations, but as the White Knight wisely explained to Alice, it is well
; to be provided for everything. Suppose you are visiting a forest in which
; every inhabitant is either a knight or a knave. (We recall that knights
; always tell the truth and knaves always lie.) In addition, some of the
; inhabitants are werewolves and have the annoying habit of sometimes turning
; into wolves at night and devouring people. A werewolf can be either a knight
; or a knave. Again, each of A,B, C is a knight or a knave and exactly one of
; them is a werewolf. They make the following statements:
;      A: I am a werewolf.
;      B: I am a werewolf.
;      C: At most one of us is a knight.4

(define liars-kb
  (list ))