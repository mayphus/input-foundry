#lang racket/base

(provide (struct-out keyboard-dimension)
         keyboard-dimensions
         keyboard-dimension-ref)

(struct keyboard-dimension
  (id
   skeleton
   projection
   interactions
   target)
  #:transparent)

(define keyboard-dimensions
  (list
   (keyboard-dimension 'standard-26
                       'standard-26
                       'identity-26
                       '(standard-mobile no-swipe-down)
                       'yuanshu)
   (keyboard-dimension 'compact-14
                       'compact-14
                       'adjacent-qwerty-14
                       '(compact-mobile no-swipe-down)
                       'yuanshu)
   (keyboard-dimension 'compact-18
                       'compact-18
                       'adjacent-qwerty-18
                       '(compact-mobile no-swipe-down)
                       'yuanshu)
   (keyboard-dimension 'shuffle-17
                       'compact-17
                       'shuffle-17
                       '(custom-mobile-pages no-swipe-down)
                       'yuanshu)
   (keyboard-dimension 'zhuyin
                       'zhuyin
                       'zhuyin-direct
                       '(zhuyin-mobile custom-mobile-pages no-swipe-down)
                       'yuanshu)))

(define keyboard-dimension-by-id
  (for/hash ([dimension (in-list keyboard-dimensions)])
    (values (keyboard-dimension-id dimension) dimension)))

(define (keyboard-dimension-ref id)
  (hash-ref keyboard-dimension-by-id
            id
            (lambda ()
              (error 'keyboard-dimension-ref "unknown keyboard dimension: ~a" id))))
