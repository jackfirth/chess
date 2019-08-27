#lang scribble/manual

@(require (for-label chess/board
                     chess/patch
                     chess/piece
                     chess/square
                     racket/base
                     racket/contract/base
                     racket/set
                     rebellion/base/result)
          (submod chess/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'chess/patch)
    #:private (list 'racket/base)))

@title{Chess Patches}
@defmodule[chess/patch]

A @deftech{chess patch} is a set of changes to a @tech{chess board}, such as
piece movements and captures. Boards can be modified with patches using @racket[
 chess-patch-apply]. Patching a chess board might fail, for example because
pieces needing to be removed aren't present or there are obstacles in the way of
added pieces. Patch application is atomic: a patch might succeed or fail but it
will never @emph{half} succeed, as failures always leave the board unchanged.

@defproc[(chess-patch? [v any/c]) boolean?]{
 A predicate for @tech{chess patches}.}

@defproc[(chess-patch
          [#:placements placements
           (hash/c chess-square? colored-chess-piece? #:immutable #t #:flat? #t)
           (hash)]
          [#:removals removals
           (hash/c chess-square? colored-chess-piece? #:immutable #t #:flat? #t)
           (hash)]
          [#:captures captures
           (hash/c chess-square? chess-color? #:immutable #t #:flat? #t)
           (hash)]
          [#:obstruction-checks obstruction-checks
           (set/c chess-square?)
           (set)]
          [#:safety-checks safety-checks
           (hash/c chess-square? chess-color? #:immutable #t #:flat? #t)
           (hash)])
         chess-patch?]{
 Constructs a @tech{chess patch} that makes the following changes when applied:

 @itemlist[
 @item{For each square and piece in @racket[placements], adds a piece to the
   board at that square. The square must be empty, but note that piece removals
   and captures in a patch are applied before piece placements.}

 @item{For each square and piece in @racket[removals], removes that piece from
   that square of the board. If the square does not contain a piece or it
   contains a different piece, the patch fails.}

 @item{For each square and capturing player color in @racket[captures], removes
   an opponent's piece from that square. If the square does not contain a piece,
   or if it contains a piece of the same color as the capturing player, then the
   patch fails.}

 @item{For each square in @racket[obstruction-checks], checks that the square is
   unoccupied. If it isn't, the patch fails. This is used to represent the
   movement of pieces like bishops and rooks, which move along lines of empty
   squares.}

 @item{For each square and defending player color in @racket[safety-checks],
   checks that that square is safe from attack by any opponent's piece. If it
   isn't, the patch fails. This is used to represent the movement of kings,
   which cannot move into squares that would place the king in check.}]}

@defproc[(chess-patch-apply [patch chess-patch?] [board chess-board?]) result?]{
 Applies @racket[patch] to @racket[board], returning a @tech{result} containing
 either the successfully-updated chess board or a reason describing why the
 patch failed.}

@defthing[empty-chess-patch chess-patch? #:value (chess-patch)]{
 The empty @tech{chess patch}, which does nothing to the board and always
 succeeds.}

@defproc[(chess-patch-failure-reason? [v any/c]) boolean?]
