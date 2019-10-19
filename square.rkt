#lang racket

(require racket/contract/base)

(provide
 (contract-out
  [chess-rank (-> (integer-in 0 7) chess-rank?)]
  [chess-rank? predicate/c]
  [chess-rank-index (-> chess-rank? (integer-in 0 7))]
  [chess-file (-> (integer-in 0 7) chess-file?)]
  [chess-file? predicate/c]
  [chess-file-index (-> chess-file? (integer-in 0 7))]
  [chess-square (-> #:rank chess-rank? #:file chess-file? chess-square?)]
  [chess-squares (set/c chess-square?)]
  [chess-square? predicate/c]
  [chess-square-rank (-> chess-square? chess-rank?)]
  [chess-square-file (-> chess-square? chess-file?)]
  [occupied-chess-square
   (-> #:rank chess-rank? #:file chess-file? #:piece colored-chess-piece?
       occupied-chess-square?)]
  [occupied-chess-square? predicate/c]
  [occupied-chess-square-rank (-> occupied-chess-square? chess-rank?)]
  [occupied-chess-square-file (-> occupied-chess-square? chess-file?)]
  [occupied-chess-square-piece (-> occupied-chess-square? colored-chess-piece?)]
  [chess-square-occupy
   (-> chess-square? colored-chess-piece? occupied-chess-square?)]
  [chess-square-remove-occupant (-> occupied-chess-square? chess-square?)]
  [in-chess-ranks (->* () (#:descending? boolean?) (sequence/c chess-rank?))]
  [in-chess-files
   (->* () (#:right-to-left? boolean?) (sequence/c chess-file?))]
  [chess-rank-delta (-> (integer-in -7 7) chess-rank-delta?)]
  [chess-rank-delta? predicate/c]
  [chess-rank-delta-value (-> chess-rank-delta? (integer-in -7 7))]
  [chess-rank+ (-> chess-rank? chess-rank-delta? chess-rank?)]
  [chess-file-delta (-> (integer-in -7 7) chess-file-delta?)]
  [chess-file-delta? predicate/c]
  [chess-file-delta-value (-> chess-file-delta? (integer-in -7 7))]
  [chess-file+ (-> chess-file? chess-file-delta? chess-file?)]
  [chess-vector
   (-> #:rank-delta chess-rank-delta? #:file-delta chess-file-delta?
       chess-vector?)]
  [chess-vector-rank-delta (-> chess-vector? chess-rank-delta?)]
  [chess-vector-file-delta (-> chess-vector? chess-file-delta?)]
  [chess-vector+ (-> chess-vector? ... chess-vector?)]
  [chess-square+
   (-> (or/c chess-square? occupied-chess-square?) chess-vector?
       (or/c chess-square? occupied-chess-square?))]
  [up chess-vector?]
  [down chess-vector?]
  [left chess-vector?]
  [right chess-vector?]
  [up-left chess-vector?]
  [up-right chess-vector?]
  [down-left chess-vector?]
  [down-right chess-vector?]
  [up-up-left chess-vector?]
  [up-up-right chess-vector?]
  [down-down-left chess-vector?]
  [down-down-right chess-vector?]
  [left-left-up chess-vector?]
  [left-left-down chess-vector?]
  [right-right-up chess-vector?]
  [right-right-down chess-vector?]))

(require (for-syntax racket/base
                     racket/syntax)
         chess/piece
         racket/sequence
         racket/set
         racket/syntax
         rebellion/base/immutable-string
         rebellion/collection/keyset
         rebellion/custom-write
         rebellion/streaming/reducer
         rebellion/type/record
         rebellion/type/tuple
         rebellion/type/wrapper
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Type definitions

(define-tuple-type chess-rank (index))
(define-tuple-type chess-file (index))

(define (make-chess-square-properties descriptor)
  (define type (record-descriptor-type descriptor))
  (define type-name (record-type-name type))
  (define accessor (record-descriptor-accessor descriptor))
  (define file-field (keyset-index-of (record-type-fields type) '#:file))
  (define rank-field (keyset-index-of (record-type-fields type) '#:rank))
  (define custom-write (make-named-object-custom-write type-name))
  (define (object-name this)
    (define rank (chess-rank-index (accessor this rank-field)))
    (define file (chess-file-index (accessor this file-field)))
    (format-symbol "~a~a" (immutable-string-ref "abcdefgh" file) (add1 rank)))
  (list (cons prop:equal+hash (default-record-equal+hash descriptor))
        (cons prop:custom-write custom-write)
        (cons prop:object-name object-name)))

(define-record-type chess-square (file rank)
  #:property-maker make-chess-square-properties)

(define-record-type occupied-chess-square (rank file piece))

;@------------------------------------------------------------------------------
;; Chess square constants

(define-simple-macro (define-chess-squares)
  #:with (unintroduced-id ...)
  (for*/list ([file-letter (in-string "abcdefgh")] [rank-number (in-range 1 9)])
    (format-symbol "~a~a" file-letter rank-number))
  #:with (id ...) (syntax-local-introduce #'(unintroduced-id ...))
  #:with (expr ...)
  (for*/list ([file-index (in-range 0 8)] [rank-index (in-range 0 8)])
    (with-syntax ([literal-rank-index rank-index]
                  [literal-file-index file-index])
      #'(chess-square #:file (chess-file 'literal-file-index)
                      #:rank (chess-rank 'literal-rank-index))))
  (begin
    (provide id ...)
    (define id expr) ...))

(define-chess-squares)

;@------------------------------------------------------------------------------
;; Chess square functions

(define (chess-square-occupy sq piece)
  (occupied-chess-square #:rank (chess-square-rank sq)
                         #:file (chess-square-file sq)
                         #:piece piece))

(define (chess-square-remove-occupant sq)
  (chess-square #:file (occupied-chess-square-file sq)
                #:rank (occupied-chess-square-rank sq)))

(define (in-chess-ranks #:descending? [descending? #f])
  (define indices (if descending? (in-range 7 -1 -1) (in-range 0 8)))
  (sequence-map chess-rank indices))

(define (in-chess-files #:right-to-left? [rtl? #f])
  (define indices (if rtl? (in-range 7 -1 -1) (in-range 0 8)))
  (sequence-map chess-file indices))

(define chess-squares
  (for*/set ([r (in-chess-ranks)] [f (in-chess-files)])
    (chess-square #:rank r #:file f)))

(module+ test
  (test-case "square occupation"
    (for* ([square (in-immutable-set chess-squares)]
           [piece (in-immutable-set colored-chess-pieces)])
      (define occupied (chess-square-occupy square piece))
      (check-equal? (chess-square-remove-occupant occupied) square))))

;@------------------------------------------------------------------------------
;; Chess vectors

(define-wrapper-type chess-rank-delta)
(define-wrapper-type chess-file-delta)

(define-record-type chess-vector (rank-delta file-delta))

(define zero-chess-vector
  (chess-vector #:rank-delta (chess-rank-delta 0)
                #:file-delta (chess-file-delta 0)))

(define (chess-vector-binary+ vec other-vec)
  (define rank-delta
    (chess-rank-delta
     (+ (chess-rank-delta-value (chess-vector-rank-delta vec))
        (chess-rank-delta-value (chess-vector-rank-delta other-vec)))))
  (define file-delta
    (chess-file-delta
     (+ (chess-file-delta-value (chess-vector-file-delta vec))
        (chess-file-delta-value (chess-vector-file-delta other-vec)))))
  (chess-vector #:rank-delta rank-delta #:file-delta file-delta))

(define into-chess-vector-sum
  (make-fold-reducer chess-vector-binary+ zero-chess-vector
                     #:name 'into-chess-vector-sum))

(define (chess-vector+ . vecs) (reduce-all into-chess-vector-sum vecs))

(define (chess-vector* vec scalar)
  (define rank-delta
    (chess-rank-delta
     (* (chess-rank-delta-value (chess-vector-rank-delta vec)) scalar)))
  (define file-delta
    (chess-file-delta
     (* (chess-file-delta-value (chess-vector-file-delta vec)) scalar)))
  (chess-vector #:rank-delta rank-delta #:file-delta file-delta))

(define up
  (chess-vector #:rank-delta (chess-rank-delta 1)
                #:file-delta (chess-file-delta 0)))

(define down
  (chess-vector #:rank-delta (chess-rank-delta -1)
                #:file-delta (chess-file-delta 0)))

(define left
  (chess-vector #:rank-delta (chess-rank-delta 0)
                #:file-delta (chess-file-delta -1)))

(define right
  (chess-vector #:rank-delta (chess-rank-delta 0)
                #:file-delta (chess-file-delta 1)))

(define up-left (chess-vector+ up left))
(define up-right (chess-vector+ up right))
(define down-left (chess-vector+ down left))
(define down-right (chess-vector+ down left))

(define up-up-left (chess-vector+ up up left))
(define up-up-right (chess-vector+ up up right))
(define down-down-left (chess-vector+ down down left))
(define down-down-right (chess-vector+ down down right))
(define left-left-up (chess-vector+ left left up))
(define left-left-down (chess-vector+ left left down))
(define right-right-up (chess-vector+ right right up))
(define right-right-down (chess-vector+ right right down))

;@------------------------------------------------------------------------------
;; Neighboring squares

(define (chess-rank+ rank delta)
  (chess-rank (+ (chess-rank-index rank) (chess-rank-delta-value delta))))

(define (chess-file+ file delta)
  (chess-file (+ (chess-file-index file) (chess-file-delta-value delta))))

(define (unoccupied-chess-square+ square vec)
  (define rank-delta (chess-vector-rank-delta vec))
  (define file-delta (chess-vector-file-delta vec))
  (chess-square #:rank (chess-rank+ (chess-square-rank square) rank-delta)
                #:file (chess-file+ (chess-square-file square) file-delta)))

(define (chess-square+ square vec)
  (cond
    [(chess-square? square) (unoccupied-chess-square+ square vec)]
    [else
     (define unoccupied (chess-square-remove-occupant square))
     (define occupant (occupied-chess-square-piece square))
     (define new-unoccupied (unoccupied-chess-square+ unoccupied vec))
     (chess-square-occupy new-unoccupied occupant)]))

(module+ test
  (test-case "chess-square+"
    (check-equal? (chess-square+ d3 up-up-right) e5)
    (check-equal? (chess-square+ h8 down) h7)
    (check-equal? (chess-square+ (chess-square-occupy e1 white-king) up-left)
                  (chess-square-occupy d2 white-king))))
