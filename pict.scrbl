#lang scribble/manual

@(require (for-label chess/board
                     chess/pict
                     chess/piece
                     chess/square
                     pict
                     racket/base
                     racket/contract/base)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess/board
                   'chess/pict
                   'chess/piece
                   'chess/square)
    #:private (list 'racket/base)))

@title{Chess Pictures}
@defmodule[chess/pict]

@defproc[(chess-board-pict [board chess-board?]
                           [#:settings settings chess-display-settings?
                            default-chess-display-settings])
         pict?]{
 Draws a picture of @racket[board], using @racket[settings] to decide what icons
 to use for each piece, what colors to use for squares, etc.}

@defproc[(chess-piece-pict [piece colored-chess-piece?]
                           [#:settings settings chess-display-settings?
                            default-chess-display-settings])
         pict?]{
 Draws a picture of @racket[piece], using @racket[settings] to decide what icons
 to use.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-piece-pict white-king)
   (chess-piece-pict black-knight)
   (chess-piece-pict white-bishop))}

@defproc[(chess-square-pict [square (or/c chess-square? occupied-chess-square?)]
                            [#:settings settings chess-display-settings?
                             default-chess-display-settings])
         pict?]{
 Draws a picture of @racket[square], including the piece occupying it if
 @racket[square] is an @tech{occupied chess square}. The color of the square is
 based on @racket[settings] and on what position the square is at, in order to
 ensure adjacent squares have different colors.

 @(examples
   #:eval (make-evaluator) #:once
   (chess-square-pict a1)
   (chess-square-pict b1)
   (chess-square-pict (chess-square-occupy a1 white-rook))
   (chess-square-pict (chess-square-occupy b1 white-knight)))}

@section{Chess Display Settings}

The drawing of chess boards and pieces is controlled by @deftech{chess display
 settings}. These settings determine what icons to use for pieces, how to draw
squares, and various other factors. At present the default settings cannot be
changed.

@defproc[(chess-display-settings? [v any/c]) boolean]{
 A predicate for @tech{chess display settings}.}

@defthing[default-chess-display-settings chess-display-settings?]{
 The default @tech{chess display settings}.

 @(examples
   #:eval (make-evaluator) #:once
   default-chess-display-settings)}
