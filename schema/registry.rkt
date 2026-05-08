#lang racket/base

(require racket/list)

(provide generated-schema-ids
         generated-custom-ids
         generated-config-ids
         extra-schema-ids-with-mobile
         schema-source-id
         static-schema-deps
         static-schema-name
         static-schema-description
         static-schema-artifacts
         schema-catalog-order
         schema-id->catalog-id
         schema-catalog-label
         schema-catalog-summary)

(define generated-schema-ids
  '("flypy"
    "flypy_14"
    "flypy_18"
    "flypy_ice"
    "luna_pinyin"
    "pinyin_14"
    "shuffle_17"
    "terra_pinyin"))

(define generated-custom-ids
  '("cangjie6"
    "flypy"
    "jyut6ping3"))

(define generated-config-ids
  (remove-duplicates (append generated-schema-ids generated-custom-ids)))

(define extra-schema-ids-with-mobile
  '("bopomofo"))

(define schema-source-ids
  (hash "flypy_ice" "flypy"))

(define (schema-source-id schema)
  (hash-ref schema-source-ids schema schema))

(define static-schema-metadata
  (hash "bopomofo" (hash 'name "注音"
                         'description "Bopomofo phonetic input for Mandarin, arranged for Yuanshu keyboard layouts."
                         'deps '()
                         'artifacts '("yuanshu"))
        "cangjie6" (hash 'name "蒼頡"
                         'description "Sixth-generation Cangjie shape input with Rime config and Yuanshu keyboard layout support."
                         'deps '("flypy")
                         'artifacts '("rime" "yuanshu"))
        "jyut6ping3" (hash 'name "粵拼"
                           'description "Jyutping Cantonese input with Cantonese dictionaries and Yuanshu keyboard layout support."
                           'deps '("flypy" "cangjie6")
                           'artifacts '("rime" "yuanshu"))))

(define (static-schema-deps schema)
  (hash-ref (hash-ref static-schema-metadata schema (hash)) 'deps '()))

(define (static-schema-name schema)
  (hash-ref (hash-ref static-schema-metadata schema (hash)) 'name #f))

(define (static-schema-description schema)
  (hash-ref (hash-ref static-schema-metadata schema (hash)) 'description #f))

(define (static-schema-artifacts schema)
  (hash-ref (hash-ref static-schema-metadata schema (hash)) 'artifacts '("rime" "yuanshu")))

(define schema-catalog-order
  '("double-pinyin" "full-pinyin" "shape" "cantonese" "phonetic" "other"))

(define (schema-id->catalog-id id)
  (cond
    [(member id '("flypy" "flypy_ice" "flypy_14" "flypy_18" "shuffle_17")) "double-pinyin"]
    [(member id '("luna_pinyin" "terra_pinyin" "pinyin_14")) "full-pinyin"]
    [(equal? id "cangjie6") "shape"]
    [(equal? id "jyut6ping3") "cantonese"]
    [(equal? id "bopomofo") "phonetic"]
    [else "other"]))

(define (schema-catalog-label catalog-id)
  (hash-ref (hash "double-pinyin" "Double Pinyin"
                  "full-pinyin" "Full Pinyin"
                  "shape" "Shape"
                  "cantonese" "Cantonese"
                  "phonetic" "Phonetic"
                  "other" "Other")
            catalog-id
            catalog-id))

(define (schema-catalog-summary catalog-id)
  (hash-ref (hash "double-pinyin" "Compact phonetic systems that trade full syllable spelling for paired initials and finals."
                  "full-pinyin" "Full Mandarin pinyin systems, useful as the baseline for modern phonetic input."
                  "shape" "Shape-based methods that encode character structure rather than pronunciation."
                  "cantonese" "Cantonese input methods and dictionaries for Jyutping-style typing."
                  "phonetic" "Keyboard layouts based on phonetic symbols rather than Latin pinyin letters."
                  "other" "Additional input experiments and supporting schemas.")
            catalog-id
            "Additional input experiments and supporting schemas."))
