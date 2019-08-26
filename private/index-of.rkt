#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [into-index-of (-> any/c reducer?)]
  [sequence-index-of (-> sequence? any/c option?)]))

(require rebellion/base/option
         rebellion/base/variant
         rebellion/streaming/reducer)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

;; TODO: add this to rebellion/streaming/reducer
;; Any -> Reducer Any (Option Natural)
(define (into-index-of v)
  (make-reducer
   #:starter (λ () (variant #:consume 0))
   #:consumer
   (λ (index-counter elem)
     (if (equal? elem v)
         (variant #:early-finish index-counter)
         (variant #:consume (add1 index-counter))))
   #:finisher (λ (_) absent)
   #:early-finisher (λ (index-counter) (present index-counter))
   #:name 'into-index-of))

(define (sequence-index-of seq v)
  (reduce-all (into-index-of v) seq))

(module+ test
  (test-case "into-index-of"
    (check-equal? (reduce-all (into-index-of #\f) "food") (present 0))
    (check-equal? (reduce-all (into-index-of #\f) "hoof") (present 3))
    (check-equal? (reduce-all (into-index-of #\f) "cat") absent)))
