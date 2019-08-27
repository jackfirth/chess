#lang scribble/manual

@(require (for-label chess/piece
                     chess/square
                     racket/base
                     racket/contract/base
                     racket/sequence)
          (submod chess/private/scribble-defthingstogether doc)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess/piece
                   'chess/square)
    #:private (list 'racket/base)))

@title{Chess Squares}
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
 Returns the @tech{chess rank} of @racket[square].}

@defproc[(chess-square-file [square chess-square?]) chess-file?]{
 Returns the @tech{chess file} of @racket[square].}

@section{Occupied Chess Squares}

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
 @racket[piece].

 @(examples
   #:eval (make-evaluator) #:once
   (chess-square-occupy d4 white-pawn))}

@defproc[(chess-square-remove-occupant [square occupied-chess-square?])
         chess-square?]{
 Removes the @tech{chess piece} currently occupying @racket[square] and returns
 the unoccupied square.

 @(examples
   #:eval (make-evaluator) #:once
   (define occupied (chess-square-occupy d4 white-pawn))
   (chess-square-remove-occupant occupied))}

@section{Ranks and Files}

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

@section{Chess Square Constants}

@defthingstogether[
 [a1 a2 a3 a4 a5 a6 a7 a8
  b1 b2 b3 b4 b5 b6 b7 b8
  c1 c2 c3 c4 c5 c6 c7 c8
  d1 d2 d3 d4 d5 d6 d7 d8
  e1 e2 e3 e4 e5 e6 e7 e8
  f1 f2 f3 f4 f5 f6 f7 f8
  g1 g2 g3 g4 g5 g6 g7 g8
  h1 h2 h3 h4 h5 h6 h7 h8]
 #:contract chess-square?]{
 Constants for each @tech{chess square}.}
