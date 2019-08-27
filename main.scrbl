#lang scribble/manual

@(require (for-label chess
                     chess/board
                     chess/pict
                     chess/piece
                     chess/square
                     pict
                     racket/base
                     racket/contract/base
                     racket/set
                     rebellion/streaming/reducer)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess
                   'racket/set
                   'rebellion/streaming/reducer)
    #:private (list 'racket/base)))

@title{Chess}
@defmodule[chess]

This library provides several functions and data structures for representing and
playing the game of chess.

@section{Chess Pieces and Colors}
@defmodule[chess/piece]

A @deftech{chess piece} is a pawn, knight, bishop, rook, queen, or king. They
come in two flavors:

@itemlist[
 @item{A @deftech{colored chess piece} is a piece associated with a particular
  player, such as a white bishop or a black king.}

 @item{An @deftech{uncolored chess piece} isn't associated with any player, and
  only represents a type of piece.}]

A @deftech{chess color} is either @racket[white] or @racket[black].

@defproc[(chess-color? [v any/c]) boolean?]{
 A predicate for @tech{chess colors}.}

@deftogether[[
 @defthing[white chess-color?]
 @defthing[black chess-color?]]]{
 Constants representing the two @tech{chess colors}.}

@defproc[(chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{chess pieces}.}

@defproc[(uncolored-chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{uncolored chess pieces}. Implies @racket[chess-piece?].}

@deftogether[[
 @defthing[pawn uncolored-chess-piece?]
 @defthing[knight uncolored-chess-piece?]
 @defthing[bishop uncolored-chess-piece?]
 @defthing[rook uncolored-chess-piece?]
 @defthing[queen uncolored-chess-piece?]
 @defthing[king uncolored-chess-piece?]]]{
 Constants representing @tech{uncolored chess pieces}.}

@defproc[(colored-chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{colored chess pieces}. Implies @racket[chess-piece?].}

@deftogether[[
 @defthing[white-pawn colored-chess-piece?]
 @defthing[white-knight colored-chess-piece?]
 @defthing[white-bishop colored-chess-piece?]
 @defthing[white-rook colored-chess-piece?]
 @defthing[white-queen colored-chess-piece?]
 @defthing[white-king colored-chess-piece?]
 @defthing[black-pawn colored-chess-piece?]
 @defthing[black-knight colored-chess-piece?]
 @defthing[black-bishop colored-chess-piece?]
 @defthing[black-rook colored-chess-piece?]
 @defthing[black-queen colored-chess-piece?]
 @defthing[black-king colored-chess-piece?]]]{
 Constants representing @tech{colored chess pieces}.}

@defproc[(colored-chess-piece [#:type type uncolored-chess-piece?]
                              [#:owner owner chess-color?])
         colored-chess-piece?]{
 Returns the @tech{colored chess piece} corresponding to a @racket[type] piece
 with @racket[owner] as its color.

 @(examples
   #:eval (make-evaluator) #:once
   (colored-chess-piece #:type pawn #:owner white))}

@defproc[(colored-chess-piece-type [piece colored-chess-piece?])
         uncolored-chess-piece?]{
 Returns the type of @racket[piece], as an @tech{uncolored chess piece}.

 @(examples
   #:eval (make-evaluator) #:once
   (colored-chess-piece-type black-knight))}

@defproc[(colored-chess-piece-owner [piece colored-chess-piece?]) chess-color?]{
 Returns the @tech{chess color} that owns @racket[piece].

 @(examples
   #:eval (make-evaluator) #:once
   (colored-chess-piece-owner white-queen))}

@defthing[uncolored-chess-pieces (set/c uncolored-chess-piece?)]{
 An immutable set of all possible @tech{uncolored chess pieces}.}

@defthing[colored-chess-pieces (set/c colored-chess-piece?)]{
 An immutable set of all possible @tech{colored chess pieces}.}

@section{Chess Boards}
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

@section{Chess Squares}
@defmodule[chess/square]

A @deftech{chess square} is a single space on a @tech{chess board}. There are 64
squares. This module exports a constant for each chess square, with a name based
on its position:

@(examples
  #:eval (make-evaluator) #:once
  a8
  d4
  c7)

@defproc[(chess-square? [v any/c]) boolean?]{
 A predicate for @tech{chess squares}.}

@defproc[(chess-square [#:rank rank chess-rank?] [#:file file chess-file?])
         chess-square?]{
 Constructs a @tech{chess square} representing the location at @racket[rank] and
 @racket[file].}

@defproc[(chess-square-rank [square chess-square?]) chess-rank?]{
 Returns the @tech{rank} of @racket[square].}

@defproc[(chess-square-file [square chess-square?]) chess-file?]{
 Returns the @tech{file} of @racket[square].}

@subsection{Occupied Chess Squares}

An @deftech{occupied chess square} is a combination of a square and a piece.
Construct one with @racket[chess-square-occupy].

@defproc[(occupied-chess-square? [v any/c]) boolean?]{
 A predicate for @tech{occupied chess squares}.}

@defproc[(occupied-chess-square-piece [square occupied-chess-square?])
         colored-chess-piece?]{
 Returns the @tech{chess piece} currently occupying @racket[square].}

@defproc[(chess-square-occupy [square chess-square?]
                              [piece colored-chess-piece?])
         occupied-chess-square?]{
 Constructs an @tech{occupied chess square} where @racket[square] is occupied by
 @racket[piece].}

@defproc[(chess-square-remove-occupant [square occupied-chess-square?])
         chess-square?]{
 Removes the @tech{chess piece} currently occupying @racket[square] and returns
 the unoccupied square.}

@subsection{Ranks and Files}

@defproc[(chess-rank? [v any/c]) boolean?]{
 A predicate for @tech{chess ranks}.}

@defproc[(chess-rank [index (integer-in 0 7)]) chess-rank?]{
 Constructs a @tech{chess rank} representing the row of spaces at position
 @racket[index], where positions 0 and 1 correspond to the ranks containing
 white's starting pieces and positions 6 and 7 correspond to black's starting
 ranks.}

@defproc[(chess-rank-index [rank chess-rank?]) (integer-in 0 7)]{
 Returns the index of @racket[rank]. See @racket[chess-rank] for details on rank
 numbering.}

@defproc[(chess-file? [v any/c]) boolean?]{
 A predicate for @tech{chess files}.}

@defproc[(chess-file [index (integer-in 0 7)]) chess-file?]{
 Constructs a @tech{chess file} representing the column of spaces at position
 @racket[index], where position 0 corresponds to the A file and position 7
 corresponds to the H file.}

@defproc[(chess-file-index [file chess-file?]) (integer-in 0 7)]{
 Returns the index of @racket[file]. See @racket[chess-file] for details on file
 numbering.}

@defproc[(in-chess-ranks [#:descending? descending? boolean? #f])
         (sequence/c chess-rank?)]{
 Returns a sequence of all @tech{chess ranks}. If @racket[descending?] is true,
 the ranks are listed from top to bottom, otherwise they are listed from bottom
 to top.}

@defproc[(in-chess-files [#:right-to-left? right-to-left? boolean? #f])
         (sequence/c chess-file?)]{
 Returns a sequence of all @tech{chess files}. If @racket[right-to-left?] is
 true, the files are listed from right to left (from the H file to the A file).
 Otherwise, they are listed from left to right.}

@section{Chess Pictures}
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

@subsection{Chess Display Settings}

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
