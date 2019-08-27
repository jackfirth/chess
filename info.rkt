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
        "rebellion"
        "reprovide-lang"))

(define build-deps
  (list "pict-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
