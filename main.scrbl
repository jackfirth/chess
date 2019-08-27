#lang scribble/manual

@(require (for-label chess))

@title{Chess}
@defmodule[chess]

This library provides several functions and data structures for representing and
playing the game of chess.

@table-of-contents[]

@include-section[(lib "chess/piece.scrbl")]
@include-section[(lib "chess/board.scrbl")]
@include-section[(lib "chess/square.scrbl")]
@include-section[(lib "chess/pict.scrbl")]
