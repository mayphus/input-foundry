#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "../core/dsl.rkt"
         "../core/visual-policy.rkt"
         "actions.rkt")

(provide hybrid-letter-specs
         key-spec-letter
         key-spec-cangjie
         key-spec-flypy
         key-spec-symbol
         key-spec-layer-text
         key-spec-swipe-up
         key-spec-swipe-down
         find-hybrid-letter-spec
         default-legend-centers)

(struct key-spec (letter cangjie flypy symbol layers swipe-up swipe-down) #:transparent)

(define (hash-ref* table key [default ""])
  (hash-ref table key (lambda () default)))

(define-syntax (define-letter-specs stx)
  (define-splicing-syntax-class maybe-swipe-down
    (pattern (~seq #:swipe-down swipe-down:expr))
    (pattern (~seq) #:attr swipe-down #'#f))
  (syntax-parse stx
    [(_ name:id
        [letter:id
         #:cangjie cangjie:expr
         #:flypy flypy:expr
         #:symbol symbol:expr
         #:swipe-up swipe-up:expr
         maybe:maybe-swipe-down] ...)
     #'(define name
         (list
          (key-spec (symbol->string 'letter)
                    cangjie
                    flypy
                    symbol
                    (hash 'wubi (hash-ref* wubi86-legends 'letter)
                          'stroke (hash-ref* stroke-legends 'letter)
                          'zrm (hash-ref* zrm-legends 'letter)
                          'abc-dp (hash-ref* abc-dp-legends 'letter)
                          'mspy (hash-ref* mspy-legends 'letter)
                          'pyjj (hash-ref* pyjj-legends 'letter)
                          'st (hash-ref* st-legends 'letter))
                    swipe-up
                    maybe.swipe-down)
          ...))]))

(define (num lexeme)
  (json-number lexeme))

(define default-legend-centers
  (hash 'abc (key-note-position 'right)
        'cangjie (key-note-position 'top-left)
        'symbol (key-note-position 'top-right)
        'wubi (key-note-position 'center)
        'stroke (key-note-position 'center)
        'zrm (key-note-position 'center)
        'abc-dp (key-note-position 'center)
        'mspy (key-note-position 'center)
        'pyjj (key-note-position 'center)
        'st (key-note-position 'center)
        'flypy-single (key-note-position 'bottom)
        'flypy-top (key-note-position 'center)
        'flypy-bottom (key-note-position 'bottom)))

(define wubi86-legends
  (hash 'q "金/勹" 'w "人/八" 'e "月/彡" 'r "白/手" 't "禾/竹"
        'y "言/文" 'u "立/辛" 'i "水/小" 'o "火/米" 'p "之/宀"
        'a "工/戈" 's "木/丁" 'd "大/犬" 'f "土/十" 'g "王/一"
        'h "目/止" 'j "日/虫" 'k "口/川" 'l "田/力"
        'z "拼音" 'x "纟/弓" 'c "又/巴" 'v "女/刀" 'b "子/耳"
        'n "已/心" 'm "山/贝"))

(define stroke-legends
  (hash 'h "一" 's "丨" 'p "丿" 'n "丶" 'z "乙"
        'j "一" 'k "丨" 'l "丿" 'u "丶" 'i "乙"))

(define zrm-legends
  (hash 'q "iu" 'w "ia/ua" 'e "e" 'r "uan" 't "ue/ve"
        'y "ing/uai" 'u "sh" 'i "ch" 'o "uo" 'p "un"
        'a "a" 's "ong" 'd "uang" 'f "en" 'g "eng"
        'h "ang" 'j "an" 'k "ao" 'l "ai"
        'z "ei" 'x "ie" 'c "iao" 'v "zh/ui" 'b "ou"
        'n "in" 'm "ian"))

(define abc-dp-legends
  (hash 'q "ei" 'w "ian" 'e "ch" 'r "er/iu" 't "iang"
        'y "ing" 'u "u" 'i "i" 'o "uo/零" 'p "uan"
        'a "zh" 's "ong" 'd "ia/ua" 'f "en" 'g "eng"
        'h "ang" 'j "an" 'k "ao" 'l "ai"
        'z "iao" 'x "ie" 'c "in/uai" 'v "sh" 'b "ou"
        'n "un" 'm "ui/ue"))

(define mspy-legends
  (hash 'q "iu" 'w "ia/ua" 'e "e" 'r "er/uan" 't "ue/ve"
        'y "v/uai" 'u "sh" 'i "ch" 'o "uo" 'p "un"
        'a "a" 's "ong" 'd "uang" 'f "en" 'g "eng"
        'h "ang" 'j "an" 'k "ao" 'l "ai"
        'z "ei" 'x "ie" 'c "iao" 'v "zh/ui" 'b "ou"
        'n "in" 'm "ian"))

(define pyjj-legends
  (hash 'q "er/ing" 'w "ei" 'e "e" 'r "en" 't "eng"
        'y "ong" 'u "ch" 'i "sh" 'o "uo" 'p "ou"
        'a "a" 's "ai" 'd "ao" 'f "an" 'g "ang"
        'h "uang" 'j "ian" 'k "iao" 'l "in"
        'z "un" 'x "ve/uai" 'c "uan" 'v "zh/ui" 'b "ia/ua"
        'n "iu" 'm "ie"))

(define st-legends
  (hash 'q "er" 'w "ei" 'e "e" 'r "en" 't "eng"
        'y "ong" 'u "ch" 'i "sh" 'o "uo" 'p "ou"
        'a "zh" 's "ai" 'd "ao" 'f "an" 'g "ang"
        'h "uang" 'j "ian" 'k "iao" 'l "in"
        'z "un" 'x "v/uai" 'c "uan" 'v "ui/ue" 'b "ia/ua"
        'n "iu" 'm "ie"))

(define-letter-specs hybrid-letter-specs
  [q #:cangjie "手" #:flypy "iu" #:symbol "1" #:swipe-up (char-action "1")]
  [w #:cangjie "田" #:flypy "ei" #:symbol "2" #:swipe-up (char-action "2")]
  [e #:cangjie "水" #:flypy "e" #:symbol "3" #:swipe-up (char-action "3")]
  [r #:cangjie "口" #:flypy "uan" #:symbol "4" #:swipe-up (char-action "4")]
  [t #:cangjie "廿" #:flypy "ue\nve" #:symbol "5" #:swipe-up (char-action "5")]
  [y #:cangjie "卜" #:flypy "un" #:symbol "6" #:swipe-up (char-action "6")]
  [u #:cangjie "山" #:flypy "sh" #:symbol "7" #:swipe-up (char-action "7")]
  [i #:cangjie "戈" #:flypy "ch" #:symbol "8" #:swipe-up (char-action "8")]
  [o #:cangjie "人" #:flypy "uo" #:symbol "9" #:swipe-up (char-action "9")]
  [p #:cangjie "心" #:flypy "ie" #:symbol "0" #:swipe-up (char-action "0")]
  [a #:cangjie "日" #:flypy "a" #:symbol "`" #:swipe-up (char-action "`")]
  [s #:cangjie "尸" #:flypy "ong\niong" #:symbol "/" #:swipe-up (char-action "/")]
  [d #:cangjie "木" #:flypy "ai" #:symbol ":" #:swipe-up (char-action ":")]
  [f #:cangjie "火" #:flypy "en" #:symbol ";" #:swipe-up (char-action ";")]
  [g #:cangjie "土" #:flypy "eng" #:symbol "(" #:swipe-up (char-action "(")]
  [h #:cangjie "的" #:flypy "ang" #:symbol "[" #:swipe-up (char-action "[")]
  [j #:cangjie "十" #:flypy "an" #:symbol "~" #:swipe-up (char-action "~")]
  [k #:cangjie "大" #:flypy "ing\nuai" #:symbol "@" #:swipe-up (char-action "@")]
  [l #:cangjie "中" #:flypy "iang\nuang" #:symbol "\"" #:swipe-up (char-action "\"")]
  [z #:cangjie "片" #:flypy "ou" #:symbol "," #:swipe-up (char-action ",")]
  [x #:cangjie "止" #:flypy "ia\nua" #:symbol "." #:swipe-up (char-action ".")]
  [c #:cangjie "金" #:flypy "ao" #:symbol "#" #:swipe-up (char-action "#")]
  [v #:cangjie "女" #:flypy "zh\nui" #:symbol "\\" #:swipe-up (char-action "\\")]
  [b #:cangjie "月" #:flypy "in" #:symbol "?" #:swipe-up (char-action "?")]
  [n #:cangjie "弓" #:flypy "iao" #:symbol "!" #:swipe-up (char-action "!")]
  [m #:cangjie "一" #:flypy "ian" #:symbol "…" #:swipe-up (symbol-action "…")])

(define (find-hybrid-letter-spec letter)
  (for/first ([spec (in-list hybrid-letter-specs)]
              #:when (string=? (key-spec-letter spec) letter))
    spec))

(define (key-spec-layer-text spec layer)
  (case layer
    [(abc) (key-spec-letter spec)]
    [(abc-uppercase) (string-upcase (key-spec-letter spec))]
    [(cangjie) (key-spec-cangjie spec)]
    [(flypy) (key-spec-flypy spec)]
    [(symbol) (key-spec-symbol spec)]
    [else (hash-ref (key-spec-layers spec) layer "")]))
