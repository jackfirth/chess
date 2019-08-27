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
   (->* () (#:right-to-left? boolean?) (sequence/c chess-file?))]))

(require (for-syntax racket/base
                     racket/syntax)
         chess/piece
         racket/sequence
         racket/set
         rebellion/type/record
         rebellion/type/tuple
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Chess squares

(define-tuple-type chess-rank (index))
(define-tuple-type chess-file (index))
(define-record-type chess-square (file rank))
(define-record-type occupied-chess-square (rank file piece))

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
