#lang scribble/manual

@(require (for-label chess/board
                     chess/pict
                     chess/piece
                     chess/square
                     racket/base
                     racket/contract/base
                     racket/set
                     rebellion/streaming/reducer)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess/board
                   'chess/pict
                   'chess/piece
                   'chess/square
                   'racket/set
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Chess Boards}
@defmodule[chess/board]

A @deftech{chess board} is an 8-by-8 rectangular grid of squares, and a
collection of @tech{chess pieces} placed on those squares.

@defproc[(chess-board? [v any/c]) boolean?]{
 A predicate for @tech{chess boards}.}

@defthing[empty-chess-board chess-board?]{
 The empty chess board, which contains no pieces.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-board-pict empty-chess-board))}

@defthing[starting-chess-board chess-board?]{
 The standard starting chess board.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-board-pict starting-chess-board))}

@defproc[(chess-board [square chess-square?]
                      [piece colored-chess-piece?]
                      ... ...)
         chess-board?]{
 Constructs a @tech{chess board} with each @racket[piece] placed on its
 corresponding @racket[square].

 @(examples
   #:eval (make-evaluator) #:once
   (define board
     (chess-board f2 white-pawn
                  g2 white-pawn
                  h2 white-pawn
                  f1 white-rook
                  g1 white-king))
   (chess-board-pict board))}

@defthing[into-chess-board reducer?]{
 A @tech{reducer} that collects @tech{occupied chess squares} into a @tech{chess
  board}.

 @(examples
   #:eval (make-evaluator) #:once
   (define board
     (for/reducer into-chess-board
                  ([rank (in-chess-ranks)]
                   [file (in-chess-files)]
                   [piece (in-immutable-set colored-chess-pieces)])
       (define square (chess-square #:rank rank #:file file))
       (chess-square-occupy square piece)))
   (chess-board-pict board))}

@defproc[(chess-board-ref [board chess-board?] [square chess-square?])
         (or/c colored-chess-piece? #f)]{
 Returns the @tech{chess piece} at @racket[square] in @racket[board], or
 @racket[#f] if the board is empty at that square.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-board-ref starting-chess-board a1)
   (chess-board-ref starting-chess-board e8)
   (chess-board-ref empty-chess-board a1))}

@defproc[(chess-board-ref-square [board chess-board?] [square chess-square?])
         (or/c chess-square? occupied-chess-square?)]{
 Like @racket[chess-board-ref], but returns an @tech{occupied chess square}
 containing the piece at @racket[square] in @racket[board]. If that square is
 empty, then @racket[square] is returned unchanged.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-board-ref-square starting-chess-board c8)
   (chess-board-ref-square starting-chess-board e4))}
