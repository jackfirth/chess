#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [chess-board (->* () #:rest squares+pieces/c chess-board?)]
  [chess-board? predicate/c]
  [chess-board-ref
   (-> chess-board? chess-square? (or/c colored-chess-piece? #f))]
  [chess-board-ref-square
   (-> chess-board? chess-square? (or/c chess-square? occupied-chess-square?))]
  [into-chess-board reducer?]
  [empty-chess-board chess-board?]
  [starting-chess-board chess-board?]))

(require chess/piece
         chess/private/slot
         chess/square
         racket/list
         racket/sequence
         racket/set
         rebellion/collection/immutable-vector
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

(define squares+pieces/c
  (or/c empty-list?
        (cons/c chess-square?
                (cons/c colored-chess-piece?
                        (recursive-contract squares+pieces/c #:flat)))))

;@------------------------------------------------------------------------------
;; Chess boards

(define-record-type chess-board (squares) #:constructor-name make-chess-board)

(define empty-chess-board
  (make-chess-board #:squares (make-immutable-vector 64 #f)))

(define (chess-square->board-index sq)
  (+ (* (chess-rank-index (chess-square-rank sq)) 8)
     (chess-file-index (chess-square-file sq))))

(define (board-index->chess-square idx)
  (define-values (r f) (quotient/remainder idx 8))
  (chess-square #:rank (chess-rank r) #:file (chess-file f)))

(define (chess-board-ref board sq)
  (immutable-vector-ref (chess-board-squares board)
                        (chess-square->board-index sq)))

(define (chess-board-ref-square board sq)
  (define piece (chess-board-ref board sq))
  (if piece (chess-square-occupy sq piece) sq))

(define (occupied-chess-square->slot sq)
  (slot (chess-square->board-index (chess-square-remove-occupant sq))
        (occupied-chess-square-piece sq)))

(define into-chess-board
  (reducer-map (slots-into-vector #:size 64 #:default #f)
               #:domain occupied-chess-square->slot
               #:range (λ (vec) (make-chess-board #:squares vec))))

(define (chess-board . squares+pieces)
  (for/reducer into-chess-board
               ([square+piece (in-slice 2 (in-list squares+pieces))])
    (define sq (first square+piece))
    (define piece (second square+piece))
    (chess-square-occupy sq piece)))

(define starting-chess-board
  (chess-board a1 white-rook
               b1 white-knight
               c1 white-bishop
               d1 white-queen
               e1 white-king
               f1 white-bishop
               g1 white-knight
               h1 white-rook

               a2 white-pawn
               b2 white-pawn
               c2 white-pawn
               d2 white-pawn
               e2 white-pawn
               f2 white-pawn
               g2 white-pawn
               h2 white-pawn

               a8 black-rook
               b8 black-knight
               c8 black-bishop
               d8 black-queen
               e8 black-king
               f8 black-bishop
               g8 black-knight
               h8 black-rook

               a7 black-pawn
               b7 black-pawn
               c7 black-pawn
               d7 black-pawn
               e7 black-pawn
               f7 black-pawn
               g7 black-pawn
               h7 black-pawn))

(module+ test
  (test-case "square<->index"
    (for ([sq (in-immutable-set chess-squares)])
      (check-equal? (board-index->chess-square (chess-square->board-index sq))
                    sq))
    (for ([idx (in-range 0 64)])
      (check-equal? (chess-square->board-index (board-index->chess-square idx))
                    idx)))
  (test-case "occupied-squares-into-chess-board"
    (define board
      (reduce into-chess-board
              (occupied-chess-square #:rank (chess-rank 0)
                                     #:file (chess-file 0)
                                     #:piece white-rook)
              (occupied-chess-square #:rank (chess-rank 1)
                                     #:file (chess-file 4)
                                     #:piece white-pawn)))
    (check-equal? (chess-board-ref board a1) white-rook)
    (check-equal? (chess-board-ref-square board e2)
                  (chess-square-occupy e2 white-pawn))
    (check-false (chess-board-ref board d2)))
  (test-case "empty-chess-board"
    (for ([sq (in-immutable-set chess-squares)])
      (check-false (chess-board-ref empty-chess-board sq))
      (check-equal? (chess-board-ref-square empty-chess-board sq) sq))))
