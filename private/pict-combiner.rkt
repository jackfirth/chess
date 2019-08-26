#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [into-vc-append (->* () (real?) reducer?)]
  [into-hc-append (->* () (real?) reducer?)]))

(require pict
         rebellion/collection/list
         rebellion/streaming/reducer)

;@------------------------------------------------------------------------------

(define (into-pict-combiner combiner [sep 0.0])
  (reducer-map into-list #:range (λ (picts) (apply combiner sep picts))))

(define (into-vc-append [sep 0.0]) (into-pict-combiner vc-append sep))
(define (into-hc-append [sep 0.0]) (into-pict-combiner hc-append sep))
