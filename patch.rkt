#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [chess-patch
   (-> #:placements (hash/c chess-square? colored-chess-piece?
                            #:immutable #t #:flat? #t)
       #:removals (hash/c chess-square? colored-chess-piece?
                          #:immutable #t #:flat? #t)
       #:captures (hash/c chess-square? chess-color?
                          #:immutable #t #:flat? #t)
       #:obstruction-checks (set/c chess-square?)
       #:safety-checks (hash/c chess-square? chess-color?
                               #:immutable #t #:flat? #t)
       chess-patch?)]
  [chess-patch? predicate/c]
  [chess-patch-placements
   (-> chess-patch? (hash/c chess-square? colored-chess-piece?
                            #:immutable #t #:flat? #t))]
  [chess-patch-removals
   (-> chess-patch? (hash/c chess-square? colored-chess-piece?
                            #:immutable #t #:flat? #t))]
  [chess-patch-captures
   (-> chess-patch? (hash/c chess-square? chess-color?
                            #:immutable #t #:flat? #t))]
  [chess-patch-obstruction-checks (-> chess-patch? (set/c chess-square?))]
  [chess-patch-safety-checks
   (-> chess-patch? (hash/c chess-square? chess-color?
                            #:immutable #t #:flat? #t))]
  [chess-patch-apply (-> chess-board? chess-patch? result?)]
  [empty-chess-patch chess-patch?]))

(require chess/board
         chess/piece
         chess/square
         racket/set
         rebellion/base/result
         rebellion/type/record
         rebellion/type/singleton
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-record-type chess-patch
  (placements removals captures obstruction-checks safety-checks)
  #:constructor-name constructor:chess-patch)

(define (chess-patch #:placements [placements (hash)]
                     #:removals [removals (hash)]
                     #:captures [captures (hash)]
                     #:obstruction-checks [obstruction-checks (set)]
                     #:safety-checks [safety-checks (hash)])
  (constructor:chess-patch #:placements placements
                           #:removals removals
                           #:captures captures
                           #:obstruction-checks obstruction-checks
                           #:safety-checks safety-checks))

(define empty-chess-patch (chess-patch))

(define (chess-patch-apply board patch)
  (success board))

(define (jump-capture square delta)
  (define unoccupied (chess-square-remove-occupant square))
  (define piece (occupied-chess-square-piece square))
  (define target (chess-square+ unoccupied delta))
  (chess-patch #:placements (hash target piece)
               #:removals (hash unoccupied piece)
               #:captures (hash target (colored-chess-piece-owner piece))))

(define (jump-move square delta)
  (define unoccupied (chess-square-remove-occupant square))
  (define piece (occupied-chess-square-piece square))
  (define target (chess-square+ unoccupied delta))
  (chess-patch #:placements (hash target piece)
               #:removals (hash unoccupied piece)
               #:obstruction-checks (set target)))

(define (pawn-advancement owner square)
  (define piece (colored-chess-piece #:type pawn #:owner owner))
  (jump-move (chess-square-occupy square piece)
             (if (equal? owner white) up down)))

(define (pawn-capture owner square direction)
  (define piece (colored-chess-piece #:type pawn #:owner owner))
  (jump-capture (chess-square-occupy square piece)
                (chess-vector+ direction (if (equal? owner white) up down))))

(define (knight-capture owner source direction)
  (define piece (colored-chess-piece #:type knight #:owner owner))
  (jump-capture (chess-square-occupy source piece) direction))
