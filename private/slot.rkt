#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [slot (-> natural? any/c slot?)]
  [slot? predicate/c]
  [slot-position (-> slot? natural?)]
  [slot-value (-> slot? any/c)]
  [slots-into-vector (->* (#:size natural?) (#:default any/c) reducer?)]
  [uninitialized uninitialized?]
  [uninitialized? predicate/c]))

(require racket/math
         rebellion/streaming/reducer
         rebellion/type/singleton
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/immutable-vector))

;@------------------------------------------------------------------------------

(define-tuple-type slot (position value))
(define-singleton-type uninitialized)

;; TODO: make size optional, use maximal slot position to determine size
(define (slots-into-vector #:size size
                           #:default [default uninitialized])
  (slots-into-sized-vector size default))

(define (slots-into-sized-vector size default)
  (make-effectful-fold-reducer
   (λ (vec s)
     (vector-set! vec (slot-position s) (slot-value s))
     vec)
   (λ () (make-vector size default))
   vector->immutable-vector
   #:name 'slots-into-vector))

(module+ test
  (test-case "slots-into-vector"
    (check-equal? (reduce (slots-into-vector #:size 5)
                          (slot 3 'a)
                          (slot 0 'b)
                          (slot 2 'c))
                  (immutable-vector 'b uninitialized 'c 'a uninitialized))))
