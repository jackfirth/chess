#lang racket/base

(require (for-syntax chess/private/index-of
                     racket/base
                     rebellion/base/immutable-string
                     rebellion/base/option)
         chess/piece
         chess/private/pict-combiner
         chess/private/slot
         pict
         racket/runtime-path
         racket/sequence
         racket/set
         racket/stream
         rebellion/base/option
         rebellion/collection/immutable-vector
         rebellion/streaming/reducer
         rebellion/type/record
         rebellion/type/reference
         rebellion/type/wrapper
         syntax/parse/define)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Chess squares

(define-wrapper-type rank)
(define-wrapper-type file)

(define-record-type square (file rank) #:constructor-name make-square)

(begin-for-syntax
  (define-syntax-class square-code
    #:attributes (expression)
    (pattern code:id
      #:do [(define code-string (symbol->immutable-string (syntax-e #'code)))]
      #:fail-when (and (< (immutable-string-length code-string) 2) #'code)
      "too few characters, expected a file letter and a rank number"
      #:fail-when (and (> (immutable-string-length code-string) 2) #'code)
      "too many characters, expected a file letter and a rank number"
      #:do [(define file-char (immutable-string-ref code-string 0))
            (define rank-char (immutable-string-ref code-string 1))
            (define file-index (sequence-index-of "abcdefgh" file-char))
            (define rank-index (sequence-index-of "12345678" rank-char))]
      #:fail-when (and (absent? file-index) #'code)
      "expected a file letter between \"a\" and \"h\""
      #:fail-when (and (absent? rank-index) #'code)
      "expected a rank number between 1 and 8"
      #:with literal-file-index (present-value file-index)
      #:with literal-rank-index (present-value rank-index)
      #:with expression
      #'(make-square #:file (file 'literal-file-index)
                     #:rank (rank 'literal-rank-index)))))

(define-simple-macro (square code:square-code) code.expression)

(define white-king-square (square e1))
(define white-queen-square (square d1))
(define black-king-square (square e8))
(define black-queen-square (square d8))

(define-record-type occupied-square (rank file piece)
  #:constructor-name occupied-square)

;; Square Chess-Piece -> Occupied-Square
(define (square-occupy sq piece)
  (occupied-square #:rank (square-rank sq)
                   #:file (square-file sq)
                   #:piece piece))

;; Occupied-Square -> Square
(define (square-unoccupy sq)
  (make-square #:file (occupied-square-file sq)
               #:rank (occupied-square-rank sq)))

;; -> Sequence Rank
(define (in-chess-ranks #:descending? [descending? #f])
  (define indices (if descending? (in-range 7 -1 -1) (in-range 0 8)))
  (sequence-map rank indices))

;; -> Sequence File
(define (in-chess-files) (sequence-map file (in-range 0 8)))

;; -> Sequence Square
(define (in-squares)
  (for*/stream ([r (in-chess-ranks)] [f (in-chess-files)])
    (make-square #:rank r #:file f)))

(module+ test
  (test-case "square occupation"
    (for* ([sq (in-squares)]
           [piece (in-immutable-set colored-chess-pieces)])
      (check-equal? (square-unoccupy (square-occupy sq piece)) sq))))

;@------------------------------------------------------------------------------
;; Chess boards

(define-record-type chess-board (squares) #:constructor-name make-chess-board)

(define empty-chess-board
  (make-chess-board #:squares (make-immutable-vector 64 #f)))

;; Square -> Integer-In [0, 64)
(define (square->index sq)
  (+ (* (rank-value (square-rank sq)) 8)
     (file-value (square-file sq))))

;; Integer-In [0, 64) -> Square
(define (index->square idx)
  (define-values (r f) (quotient/remainder idx 8))
  (make-square #:rank (rank r) #:file (file f)))

;; Chess-Board Square -> (U Chess-Piece #f)
(define (chess-board-ref board sq)
  (immutable-vector-ref (chess-board-squares board) (square->index sq)))

;; Chess-Board Square -> (U Square Occupied-Square)
(define (chess-board-ref-square board sq)
  (define piece (chess-board-ref board sq))
  (if piece (square-occupy sq piece) sq))

(define (occupied-square->slot sq)
  (slot (square->index (square-unoccupy sq)) (occupied-square-piece sq)))

(define occupied-squares-into-chess-board
  (reducer-map (slots-into-vector #:size 64 #:default #f)
               #:domain occupied-square->slot
               #:range (λ (vec) (make-chess-board #:squares vec))))

(define-simple-macro (chess-board (~seq sq:square-code piece:expr) ...)
  (reduce occupied-squares-into-chess-board
          (square-occupy (square sq) piece) ...))

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
    (for ([sq (in-squares)])
      (check-equal? (index->square (square->index sq)) sq))
    (for ([idx (in-range 0 64)])
      (check-equal? (square->index (index->square idx)) idx)))
  (test-case "occupied-squares-into-chess-board"
    (define board
      (reduce occupied-squares-into-chess-board
              (occupied-square #:rank (rank 0) #:file (file 0) #:piece rook)
              (occupied-square #:rank (rank 0) #:file (file 4) #:piece king)
              (occupied-square #:rank (rank 1) #:file (file 4) #:piece pawn)))
    (check-equal? (chess-board-ref board (square a1)) rook)
    (check-equal? (chess-board-ref-square board (square e2))
                  (square-occupy (square e2) pawn))
    (check-false (chess-board-ref board (square d2))))
  (test-case "empty-chess-board"
    (for ([sq (in-squares)])
      (check-false (chess-board-ref empty-chess-board sq))
      (check-equal? (chess-board-ref-square empty-chess-board sq) sq))))

;@------------------------------------------------------------------------------
;; Drawing chess boards

(define-record-type chess-board-pict-options
  (white-pawn-pict
   white-knight-pict
   white-bishop-pict
   white-rook-pict
   white-queen-pict
   white-king-pict
   black-pawn-pict
   black-knight-pict
   black-bishop-pict
   black-rook-pict
   black-queen-pict
   black-king-pict
   cell-pict
   alternate-cell-pict))

(define default-square-size 32)
(define default-png-scale-factor 1/3)

(define (load-chess-piece-png path)
  (scale (bitmap path) default-png-scale-factor))

(define-runtime-path white-pawn.png '(lib "chess/private/white-pawn.png"))
(define-runtime-path white-knight.png '(lib "chess/private/white-knight.png"))
(define-runtime-path white-bishop.png '(lib "chess/private/white-bishop.png"))
(define-runtime-path white-rook.png '(lib "chess/private/white-rook.png"))
(define-runtime-path white-queen.png '(lib "chess/private/white-queen.png"))
(define-runtime-path white-king.png '(lib "chess/private/white-king.png"))
(define-runtime-path black-pawn.png '(lib "chess/private/black-pawn.png"))
(define-runtime-path black-knight.png '(lib "chess/private/black-knight.png"))
(define-runtime-path black-bishop.png '(lib "chess/private/black-bishop.png"))
(define-runtime-path black-rook.png '(lib "chess/private/black-rook.png"))
(define-runtime-path black-queen.png '(lib "chess/private/black-queen.png"))
(define-runtime-path black-king.png '(lib "chess/private/black-king.png"))

(define default-white-pawn-pict (load-chess-piece-png white-pawn.png))
(define default-white-knight-pict (load-chess-piece-png white-knight.png))
(define default-white-bishop-pict (load-chess-piece-png white-bishop.png))
(define default-white-rook-pict (load-chess-piece-png white-rook.png))
(define default-white-queen-pict (load-chess-piece-png white-queen.png))
(define default-white-king-pict (load-chess-piece-png white-king.png))
(define default-black-pawn-pict (load-chess-piece-png black-pawn.png))
(define default-black-knight-pict (load-chess-piece-png black-knight.png))
(define default-black-bishop-pict (load-chess-piece-png black-bishop.png))
(define default-black-rook-pict (load-chess-piece-png black-rook.png))
(define default-black-queen-pict (load-chess-piece-png black-queen.png))
(define default-black-king-pict (load-chess-piece-png black-king.png))

(define default-cell-pict
  (filled-rectangle default-square-size default-square-size
                    #:draw-border? #f
                    #:color "white"))

(define default-alternate-cell-pict
  (filled-rectangle default-square-size default-square-size
                    #:draw-border? #f
                    #:color "gray"))

(define default-chess-board-pict-options
  (chess-board-pict-options
   #:white-pawn-pict default-white-pawn-pict
   #:white-knight-pict default-white-knight-pict
   #:white-bishop-pict default-white-bishop-pict
   #:white-rook-pict default-white-rook-pict
   #:white-queen-pict default-white-queen-pict
   #:white-king-pict default-white-king-pict
   #:black-pawn-pict default-black-pawn-pict
   #:black-knight-pict default-black-knight-pict
   #:black-bishop-pict default-black-bishop-pict
   #:black-rook-pict default-black-rook-pict
   #:black-queen-pict default-black-queen-pict
   #:black-king-pict default-black-king-pict
   #:cell-pict default-cell-pict
   #:alternate-cell-pict default-alternate-cell-pict))

(define (chess-board-pict-options-piece-pict options piece)
  (define pict-field
    (cond
      [(equal? piece white-pawn) chess-board-pict-options-white-pawn-pict]
      [(equal? piece white-knight) chess-board-pict-options-white-knight-pict]
      [(equal? piece white-bishop) chess-board-pict-options-white-bishop-pict]
      [(equal? piece white-rook) chess-board-pict-options-white-rook-pict]
      [(equal? piece white-queen) chess-board-pict-options-white-queen-pict]
      [(equal? piece white-king) chess-board-pict-options-white-king-pict]
      [(equal? piece black-pawn) chess-board-pict-options-black-pawn-pict]
      [(equal? piece black-knight) chess-board-pict-options-black-knight-pict]
      [(equal? piece black-bishop) chess-board-pict-options-black-bishop-pict]
      [(equal? piece black-rook) chess-board-pict-options-black-rook-pict]
      [(equal? piece black-queen) chess-board-pict-options-black-queen-pict]
      [else chess-board-pict-options-black-king-pict]))
  (pict-field options))

;; (U Square Occupied-Square) [Chess-Board-Pict-Options] -> Pict
(define (chess-board-square-pict square
                                 [options default-chess-board-pict-options])
  (define piece-pict
    (cond
      [(occupied-square? square)
       (define piece (occupied-square-piece square))
       (define pict (chess-board-pict-options-piece-pict options piece))
       (present pict)]
      [else absent]))
  (define unoccupied-square
    (if (occupied-square? square)
        (square-unoccupy square) square))
  (define rank-index (rank-value (square-rank unoccupied-square)))
  (define file-index (file-value (square-file unoccupied-square)))
  (define base
    (if (or (and (even? rank-index) (odd? file-index))
            (and (odd? rank-index) (even? file-index)))
        (chess-board-pict-options-cell-pict options)
        (chess-board-pict-options-alternate-cell-pict options)))
  (option-case piece-pict
               #:present (λ (p) (cc-superimpose base p))
               #:absent (λ () base)))

(define (chess-board-pict board [options default-chess-board-pict-options])
  (for/reducer (into-vc-append) ([rank (in-chess-ranks #:descending? #t)])
    (for/reducer (into-hc-append) ([file (in-chess-files)])
      (define sq
        (chess-board-ref-square board (make-square #:rank rank #:file file)))
      (chess-board-square-pict sq options))))

(module+ main
  (chess-board-pict starting-chess-board))
