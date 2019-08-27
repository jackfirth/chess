#lang scribble/manual

@(require (for-label chess/piece
                     racket/base
                     racket/contract/base
                     racket/set)
          (submod chess/private/scribble-defthingstogether doc)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess/piece)
    #:private (list 'racket/base)))

@title{Chess Pieces and Colors}
@defmodule[chess/piece]

A @deftech{chess piece} is a pawn, knight, bishop, rook, queen, or king. They
come in two flavors:

@itemlist[
 @item{A @deftech{colored chess piece} is a piece associated with a particular
  player, such as a white bishop or a black king.}

 @item{An @deftech{uncolored chess piece} isn't associated with any player, and
  only represents a type of piece.}]

@defproc[(chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{chess pieces}.}

@section{Uncolored Chess Pieces}

@defproc[(uncolored-chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{uncolored chess pieces}. Implies @racket[chess-piece?].}

@defthingstogether[
 [pawn knight bishop rook queen king] #:contract uncolored-chess-piece?]{
 Constants representing @tech{uncolored chess pieces}.}

@defthing[uncolored-chess-pieces (set/c uncolored-chess-piece?)]{
 An immutable set of all possible @tech{uncolored chess pieces}.}

@section{Colored Chess Pieces}

@defproc[(colored-chess-piece? [v any/c]) boolean?]{
 A predicate for @tech{colored chess pieces}. Implies @racket[chess-piece?].}

@defthingstogether[
 [white-pawn white-knight white-bishop white-rook white-queen white-king
  black-pawn black-knight black-bishop black-rook black-queen black-king]
 #:contract colored-chess-piece?]{
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

@defthing[colored-chess-pieces (set/c colored-chess-piece?)]{
 An immutable set of all possible @tech{colored chess pieces}.}

@section{Chess Colors}

A @deftech{chess color} is either @racket[white] or @racket[black].

@defproc[(chess-color? [v any/c]) boolean?]{
 A predicate for @tech{chess colors}.}

@defthingstogether[[white black] #:contract chess-color?]{
 Constants representing the two @tech{chess colors}.}

