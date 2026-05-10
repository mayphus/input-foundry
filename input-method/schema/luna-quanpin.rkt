#lang racket/base
(require "../define-schema.rkt")

(define-schema "luna-quanpin" #:category "full-pinyin"
         #:en-name "Luna Quanpin" #:zh-name "全拼"
         #:en-description "Supporting full-pinyin reverse-lookup schema used by upstream Cangjie and Quick packages."
         #:zh-description "上游倉頡與速成方案使用的全拼反查支援方案。")
