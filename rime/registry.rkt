#lang racket/base

(require racket/list)

(provide rime-schema-ids
         generated-schema-ids
         generated-package-ids
         generated-custom-ids
         generated-config-ids
         extra-schema-ids-with-mobile
         rime-schema-source-id
         rime-schema-config-id
         rime-schema-deps
         rime-schema-extra-files
         rime-schema-extra-dirs
         rime-schema-keyboard-layouts
         rime-schema-artifacts)

(struct rime-entry
  (id source-id config-id generated? package? custom? deps extra-files extra-dirs keyboard-layouts artifacts)
  #:transparent)

(define mobile-rime-artifacts '("rime" "yuanshu"))

(define (entry id
               #:source-id [source-id id]
               #:config-id [config-id source-id]
               #:generated? [generated? #f]
               #:package? [package? #f]
               #:custom? [custom? #f]
               #:deps [deps '()]
               #:extra-files [extra-files '()]
               #:extra-dirs [extra-dirs '()]
               #:keyboard-layouts [keyboard-layouts '()]
               #:artifacts [artifacts mobile-rime-artifacts])
  (rime-entry id source-id config-id generated? package? custom? deps extra-files extra-dirs keyboard-layouts artifacts))

(define rime-entries
  (list
   (entry "double-pinyin-flypy" #:source-id "flypy" #:generated? #t #:custom? #t)
   (entry "double-pinyin-flypy-14" #:source-id "flypy_14" #:generated? #t)
   (entry "double-pinyin-flypy-18" #:source-id "flypy_18" #:generated? #t)
   (entry "double-pinyin-flypy-shuffle-17" #:source-id "shuffle_17" #:generated? #t)
   (entry "luna-pinyin" #:source-id "luna_pinyin" #:generated? #t)
   (entry "terra-pinyin" #:source-id "terra_pinyin" #:generated? #t)
   (entry "pinyin-14" #:source-id "pinyin_14" #:generated? #t)
   (entry "cangjie6" #:generated? #t #:custom? #t #:deps '("double-pinyin-flypy"))
   (entry "jyut6ping3" #:generated? #t #:custom? #t #:deps '("double-pinyin-flypy" "cangjie6"))
   (entry "bopomofo-standard" #:source-id "bopomofo-standard" #:config-id "bopomofo"
          #:extra-files '("terra_pinyin.dict.yaml" "zhuyin.yaml")
          #:keyboard-layouts '("bopomofo_standard")
          #:artifacts '("rime"))
   (entry "bopomofo" #:generated? #t #:artifacts '("yuanshu"))

   (entry "double-pinyin" #:source-id "double_pinyin" #:deps '("stroke")
          #:keyboard-layouts '("double_pinyin_zrm"))
   (entry "double-pinyin-abc" #:source-id "double_pinyin_abc" #:deps '("stroke")
          #:keyboard-layouts '("double_pinyin_abc"))
   (entry "double-pinyin-mspy" #:source-id "double_pinyin_mspy" #:deps '("stroke")
          #:keyboard-layouts '("double_pinyin_mspy"))
   (entry "double-pinyin-pyjj" #:source-id "double_pinyin_pyjj" #:deps '("stroke")
          #:keyboard-layouts '("double_pinyin_pyjj"))
   (entry "double-pinyin-st" #:source-id "double_pinyin_st" #:deps '("stroke")
          #:keyboard-layouts '("double_pinyin_st"))
   (entry "cangjie5" #:deps '("luna-pinyin")
          #:extra-files '("cangjie5.dict.yaml")
          #:keyboard-layouts '("cangjie6"))
   (entry "cangjie5-express" #:source-id "cangjie5_express" #:deps '("luna-pinyin")
          #:extra-files '("cangjie5.dict.yaml")
          #:keyboard-layouts '("cangjie6"))
   (entry "wubi86" #:deps '("pinyin-simp")
          #:extra-files '("wubi86.dict.yaml")
          #:keyboard-layouts '("wubi86"))
   (entry "wubi-pinyin" #:source-id "wubi_pinyin" #:deps '("pinyin-simp")
          #:extra-files '("wubi86.dict.yaml")
          #:keyboard-layouts '("wubi86"))
   (entry "wubi-trad" #:source-id "wubi_trad" #:deps '("pinyin-simp")
          #:extra-files '("wubi86.dict.yaml")
          #:keyboard-layouts '("wubi86"))
   (entry "quick5" #:deps '("luna-pinyin")
          #:extra-files '("quick5.dict.yaml")
          #:keyboard-layouts '("cangjie6"))
   (entry "stroke" #:deps '("luna-pinyin")
          #:extra-files '("stroke.dict.yaml")
          #:keyboard-layouts '("stroke"))
   (entry "pinyin-simp" #:source-id "pinyin_simp" #:deps '("stroke")
          #:extra-files '("pinyin_simp.dict.yaml")
          #:keyboard-layouts '("luna_pinyin"))))

(define rime-entry-by-id
  (for/hash ([definition (in-list rime-entries)])
    (values (rime-entry-id definition) definition)))

(define (rime-ref id)
  (hash-ref rime-entry-by-id id #f))

(define (rime-schema-ids)
  (map rime-entry-id rime-entries))

(define (filter-rime-ids pred?)
  (for/list ([definition (in-list rime-entries)]
             #:when (pred? definition))
    (rime-entry-id definition)))

(define generated-schema-ids
  (filter-rime-ids rime-entry-generated?))

(define generated-package-ids
  (filter-rime-ids rime-entry-package?))

(define generated-custom-ids
  (filter-rime-ids rime-entry-custom?))

(define generated-config-ids
  (remove-duplicates (append generated-schema-ids
                             generated-package-ids
                             generated-custom-ids)))

(define extra-schema-ids-with-mobile
  '("bopomofo"))

(define (rime-schema-source-id schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-source-id definition) schema))

(define (rime-schema-config-id schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-config-id definition) (rime-schema-source-id schema)))

(define (rime-schema-deps schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-deps definition) '()))

(define (rime-schema-extra-files schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-extra-files definition) '()))

(define (rime-schema-extra-dirs schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-extra-dirs definition) '()))

(define (rime-schema-keyboard-layouts schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-keyboard-layouts definition) '()))

(define (rime-schema-artifacts schema)
  (define definition (rime-ref schema))
  (if definition (rime-entry-artifacts definition) mobile-rime-artifacts))
