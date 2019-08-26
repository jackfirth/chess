#lang scribble/manual

@(require (for-label chess
                     chess/piece
                     racket/base
                     racket/contract/base
                     racket/set)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess
                   'chess/piece)
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
