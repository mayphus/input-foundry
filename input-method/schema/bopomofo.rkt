#lang racket/base

(require "../define-schema.rkt")

(define-schema "bopomofo" #:category "phonetic"
         #:en-name "Bopomofo" #:zh-name "注音"
         #:en-description "Bopomofo phonetic input for Mandarin, arranged for Yuanshu keyboard layouts."
         #:zh-description "注音符號普通話輸入，配置為元書鍵盤佈局。")
