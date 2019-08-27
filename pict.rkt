#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [chess-board-pict
   (->* (chess-board?) (#:settings chess-display-settings?) pict?)]
  [chess-piece-pict
   (->* (colored-chess-piece?) (#:settings chess-display-settings?) pict?)]
  [chess-square-pict
   (->* ((or/c chess-square? occupied-chess-square?))
        (#:settings chess-display-settings?)
        pict?)]
  [chess-display-settings? predicate/c]
  [default-chess-display-settings chess-display-settings?]))

(require (for-syntax racket/base)
         chess/board
         chess/piece
         chess/private/pict-combiner
         chess/square
         pict
         racket/runtime-path
         rebellion/base/option
         rebellion/streaming/reducer
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------
;; Drawing chess boards

(define-record-type chess-display-settings
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

(define default-chess-display-settings
  (chess-display-settings
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

(define (chess-piece-pict piece
                          #:settings [settings default-chess-display-settings])
  (define pict-field
    (cond
      [(equal? piece white-pawn) chess-display-settings-white-pawn-pict]
      [(equal? piece white-knight) chess-display-settings-white-knight-pict]
      [(equal? piece white-bishop) chess-display-settings-white-bishop-pict]
      [(equal? piece white-rook) chess-display-settings-white-rook-pict]
      [(equal? piece white-queen) chess-display-settings-white-queen-pict]
      [(equal? piece white-king) chess-display-settings-white-king-pict]
      [(equal? piece black-pawn) chess-display-settings-black-pawn-pict]
      [(equal? piece black-knight) chess-display-settings-black-knight-pict]
      [(equal? piece black-bishop) chess-display-settings-black-bishop-pict]
      [(equal? piece black-rook) chess-display-settings-black-rook-pict]
      [(equal? piece black-queen) chess-display-settings-black-queen-pict]
      [else chess-display-settings-black-king-pict]))
  (pict-field settings))

(define (chess-square-pict square
                           #:settings [settings default-chess-display-settings])
  (define piece-pict
    (cond
      [(occupied-chess-square? square)
       (define piece (occupied-chess-square-piece square))
       (define pict (chess-piece-pict piece #:settings settings))
       (present pict)]
      [else absent]))
  (define unoccupied-square
    (if (occupied-chess-square? square)
        (chess-square-remove-occupant square) square))
  (define rank (chess-rank-index (chess-square-rank unoccupied-square)))
  (define file (chess-file-index (chess-square-file unoccupied-square)))
  (define base
    (if (or (and (even? rank) (odd? file))
            (and (odd? rank) (even? file)))
        (chess-display-settings-cell-pict settings)
        (chess-display-settings-alternate-cell-pict settings)))
  (option-case piece-pict
               #:present (λ (p) (cc-superimpose base p))
               #:absent (λ () base)))

(define (chess-board-pict board
                          #:settings [settings default-chess-display-settings])
  (for/reducer (into-vc-append) ([rank (in-chess-ranks #:descending? #t)])
    (for/reducer (into-hc-append) ([file (in-chess-files)])
      (define sq
        (chess-board-ref-square board (chess-square #:rank rank #:file file)))
      (chess-square-pict sq #:settings settings))))

(module+ main
  (chess-board-pict starting-chess-board))
