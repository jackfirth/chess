#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [chess-color? predicate/c]
  [white chess-color?]
  [black chess-color?]
  [chess-piece? predicate/c]
  [uncolored-chess-piece? predicate/c]
  [uncolored-chess-pieces (set/c uncolored-chess-piece?)]
  [pawn uncolored-chess-piece?]
  [knight uncolored-chess-piece?]
  [bishop uncolored-chess-piece?]
  [rook uncolored-chess-piece?]
  [queen uncolored-chess-piece?]
  [king uncolored-chess-piece?]
  [colored-chess-piece? predicate/c]
  [white-pawn colored-chess-piece?]
  [white-knight colored-chess-piece?]
  [white-bishop colored-chess-piece?]
  [white-rook colored-chess-piece?]
  [white-queen colored-chess-piece?]
  [white-king colored-chess-piece?]
  [black-pawn colored-chess-piece?]
  [black-knight colored-chess-piece?]
  [black-bishop colored-chess-piece?]
  [black-rook colored-chess-piece?]
  [black-queen colored-chess-piece?]
  [black-king colored-chess-piece?]
  [colored-chess-piece
   (-> #:type uncolored-chess-piece? #:owner chess-color? colored-chess-piece?)]
  [colored-chess-piece-type (-> colored-chess-piece? uncolored-chess-piece?)]
  [colored-chess-piece-owner (-> colored-chess-piece? chess-color?)]
  [colored-chess-pieces (set/c colored-chess-piece?)]))
  

(require racket/set
         racket/splicing
         rebellion/type/record
         rebellion/type/singleton)

;@------------------------------------------------------------------------------
;; Pieces and colors

(define-singleton-type white)
(define-singleton-type black)

(define chess-colors (set white black))
(define (chess-color? v) (set-member? chess-colors v))

(define-singleton-type pawn)
(define-singleton-type knight)
(define-singleton-type bishop)
(define-singleton-type rook)
(define-singleton-type queen)
(define-singleton-type king)

(define uncolored-chess-pieces (set pawn knight bishop rook queen king))
(define (uncolored-chess-piece? v) (set-member? uncolored-chess-pieces v))

(define-record-type colored-chess-piece (type owner))

(splicing-local
    ;; Local helpers to make the following definitions easy to read
    [(define (white! piece) (colored-chess-piece #:type piece #:owner white))
     (define (black! piece) (colored-chess-piece #:type piece #:owner black))]
  (define white-pawn (white! pawn))
  (define white-knight (white! knight))
  (define white-bishop (white! bishop))
  (define white-rook (white! rook))
  (define white-queen (white! queen))
  (define white-king (white! king))
  (define black-pawn (black! pawn))
  (define black-knight (black! knight))
  (define black-bishop (black! bishop))
  (define black-rook (black! rook))
  (define black-queen (black! queen))
  (define black-king (black! king)))

(define colored-chess-pieces
  (for*/set ([color (in-immutable-set chess-colors)]
             [piece (in-immutable-set uncolored-chess-pieces)])
    (colored-chess-piece #:type piece #:owner color)))

(define chess-pieces (set-union colored-chess-pieces uncolored-chess-pieces))
(define (chess-piece? v) (set-member? chess-pieces v))
