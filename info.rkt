#lang info

(define collection "chess")

(define scribblings
  (list (list "main.scrbl"
              (list)
              (list "Chess")
              "chess")))

(define deps
  (list "base"
        "pict-lib"
        "rebellion"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
