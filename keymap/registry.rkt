#lang racket/base

(provide keymap-definitions
         keymap-ref
         keymap-text
         keyboard-legend-definitions
         keyboard-legend-definition-ref
         keyboard-legend-text)

(define (legend-entry key text)
  (cons key text))

(define (legend-layer layer . entries)
  (cons layer (make-immutable-hash entries)))

(define (keyboard-legends . layers)
  (make-immutable-hash layers))

(define keyboard-legend-definitions
  (keyboard-legends
   (legend-layer 'abc
    (legend-entry 'q "q") (legend-entry 'w "w") (legend-entry 'e "e") (legend-entry 'r "r") (legend-entry 't "t")
    (legend-entry 'y "y") (legend-entry 'u "u") (legend-entry 'i "i") (legend-entry 'o "o") (legend-entry 'p "p")
    (legend-entry 'a "a") (legend-entry 's "s") (legend-entry 'd "d") (legend-entry 'f "f") (legend-entry 'g "g")
    (legend-entry 'h "h") (legend-entry 'j "j") (legend-entry 'k "k") (legend-entry 'l "l")
    (legend-entry 'z "z") (legend-entry 'x "x") (legend-entry 'c "c") (legend-entry 'v "v") (legend-entry 'b "b")
    (legend-entry 'n "n") (legend-entry 'm "m"))
   (legend-layer 'flypy
    (legend-entry 'q "iu") (legend-entry 'w "ei") (legend-entry 'e "e") (legend-entry 'r "uan") (legend-entry 't "ue/ve")
    (legend-entry 'y "un") (legend-entry 'u "sh") (legend-entry 'i "ch") (legend-entry 'o "uo") (legend-entry 'p "ie")
    (legend-entry 'a "a") (legend-entry 's "ong/iong") (legend-entry 'd "ai") (legend-entry 'f "en") (legend-entry 'g "eng")
    (legend-entry 'h "ang") (legend-entry 'j "an") (legend-entry 'k "ing/uai") (legend-entry 'l "iang/uang")
    (legend-entry 'z "ou") (legend-entry 'x "ia/ua") (legend-entry 'c "ao") (legend-entry 'v "zh/ui") (legend-entry 'b "in")
    (legend-entry 'n "iao") (legend-entry 'm "ian"))
   (legend-layer 'cangjie
    (legend-entry 'q "手") (legend-entry 'w "田") (legend-entry 'e "水") (legend-entry 'r "口") (legend-entry 't "廿")
    (legend-entry 'y "卜") (legend-entry 'u "山") (legend-entry 'i "戈") (legend-entry 'o "人") (legend-entry 'p "心")
    (legend-entry 'a "日") (legend-entry 's "尸") (legend-entry 'd "木") (legend-entry 'f "火") (legend-entry 'g "土")
    (legend-entry 'h "的") (legend-entry 'j "十") (legend-entry 'k "大") (legend-entry 'l "中")
    (legend-entry 'z "片") (legend-entry 'x "止") (legend-entry 'c "金") (legend-entry 'v "女") (legend-entry 'b "月")
    (legend-entry 'n "弓") (legend-entry 'm "一"))
   (legend-layer 'zhuyin
    (legend-entry 'bo "ㄅ") (legend-entry 'po "ㄆ") (legend-entry 'mo "ㄇ") (legend-entry 'fo "ㄈ")
    (legend-entry 'de "ㄉ") (legend-entry 'te "ㄊ") (legend-entry 'ne "ㄋ") (legend-entry 'le "ㄌ")
    (legend-entry 'ge "ㄍ") (legend-entry 'ke "ㄎ") (legend-entry 'he "ㄏ")
    (legend-entry 'ji "ㄐ") (legend-entry 'qi "ㄑ") (legend-entry 'xi "ㄒ")
    (legend-entry 'zhi "ㄓ") (legend-entry 'chi "ㄔ") (legend-entry 'shi "ㄕ") (legend-entry 'ri "ㄖ")
    (legend-entry 'zi "ㄗ") (legend-entry 'ci "ㄘ") (legend-entry 'si "ㄙ")
    (legend-entry 'yi "ㄧ") (legend-entry 'wu "ㄨ") (legend-entry 'yu "ㄩ")
    (legend-entry 'a "ㄚ") (legend-entry 'o "ㄛ") (legend-entry 'e "ㄜ") (legend-entry 'eh "ㄝ")
    (legend-entry 'ai "ㄞ") (legend-entry 'ei "ㄟ") (legend-entry 'ao "ㄠ") (legend-entry 'ou "ㄡ")
    (legend-entry 'an "ㄢ") (legend-entry 'en "ㄣ") (legend-entry 'ang "ㄤ") (legend-entry 'eng "ㄥ")
    (legend-entry 'er "ㄦ") (legend-entry 'second-tone "ˊ") (legend-entry 'third-tone "ˇ")
    (legend-entry 'fourth-tone "ˋ") (legend-entry 'light-tone "˙"))
   (legend-layer 'wubi
    (legend-entry 'q "金/勹") (legend-entry 'w "人/八") (legend-entry 'e "月/彡") (legend-entry 'r "白/手") (legend-entry 't "禾/竹")
    (legend-entry 'y "言/文") (legend-entry 'u "立/辛") (legend-entry 'i "水/小") (legend-entry 'o "火/米") (legend-entry 'p "之/宀")
    (legend-entry 'a "工/戈") (legend-entry 's "木/丁") (legend-entry 'd "大/犬") (legend-entry 'f "土/十") (legend-entry 'g "王/一")
    (legend-entry 'h "目/止") (legend-entry 'j "日/虫") (legend-entry 'k "口/川") (legend-entry 'l "田/力")
    (legend-entry 'z "拼音") (legend-entry 'x "纟/弓") (legend-entry 'c "又/巴") (legend-entry 'v "女/刀") (legend-entry 'b "子/耳")
    (legend-entry 'n "已/心") (legend-entry 'm "山/贝"))
   (legend-layer 'stroke
    (legend-entry 'h "一") (legend-entry 's "丨") (legend-entry 'p "丿") (legend-entry 'n "丶") (legend-entry 'z "乙")
    (legend-entry 'j "一") (legend-entry 'k "丨") (legend-entry 'l "丿") (legend-entry 'u "丶") (legend-entry 'i "乙"))
   (legend-layer 'zrm
    (legend-entry 'q "iu") (legend-entry 'w "ia/ua") (legend-entry 'e "e") (legend-entry 'r "uan") (legend-entry 't "ue/ve")
    (legend-entry 'y "ing/uai") (legend-entry 'u "sh") (legend-entry 'i "ch") (legend-entry 'o "uo") (legend-entry 'p "un")
    (legend-entry 'a "a") (legend-entry 's "ong") (legend-entry 'd "uang") (legend-entry 'f "en") (legend-entry 'g "eng")
    (legend-entry 'h "ang") (legend-entry 'j "an") (legend-entry 'k "ao") (legend-entry 'l "ai")
    (legend-entry 'z "ei") (legend-entry 'x "ie") (legend-entry 'c "iao") (legend-entry 'v "zh/ui") (legend-entry 'b "ou")
    (legend-entry 'n "in") (legend-entry 'm "ian"))
   (legend-layer 'abc-dp
    (legend-entry 'q "ei") (legend-entry 'w "ian") (legend-entry 'e "ch") (legend-entry 'r "er/iu") (legend-entry 't "iang")
    (legend-entry 'y "ing") (legend-entry 'u "u") (legend-entry 'i "i") (legend-entry 'o "uo/零") (legend-entry 'p "uan")
    (legend-entry 'a "zh") (legend-entry 's "ong") (legend-entry 'd "ia/ua") (legend-entry 'f "en") (legend-entry 'g "eng")
    (legend-entry 'h "ang") (legend-entry 'j "an") (legend-entry 'k "ao") (legend-entry 'l "ai")
    (legend-entry 'z "iao") (legend-entry 'x "ie") (legend-entry 'c "in/uai") (legend-entry 'v "sh") (legend-entry 'b "ou")
    (legend-entry 'n "un") (legend-entry 'm "ui/ue"))
   (legend-layer 'mspy
    (legend-entry 'q "iu") (legend-entry 'w "ia/ua") (legend-entry 'e "e") (legend-entry 'r "er/uan") (legend-entry 't "ue/ve")
    (legend-entry 'y "v/uai") (legend-entry 'u "sh") (legend-entry 'i "ch") (legend-entry 'o "uo") (legend-entry 'p "un")
    (legend-entry 'a "a") (legend-entry 's "ong") (legend-entry 'd "uang") (legend-entry 'f "en") (legend-entry 'g "eng")
    (legend-entry 'h "ang") (legend-entry 'j "an") (legend-entry 'k "ao") (legend-entry 'l "ai")
    (legend-entry 'z "ei") (legend-entry 'x "ie") (legend-entry 'c "iao") (legend-entry 'v "zh/ui") (legend-entry 'b "ou")
    (legend-entry 'n "in") (legend-entry 'm "ian"))
   (legend-layer 'pyjj
    (legend-entry 'q "er/ing") (legend-entry 'w "ei") (legend-entry 'e "e") (legend-entry 'r "en") (legend-entry 't "eng")
    (legend-entry 'y "ong") (legend-entry 'u "ch") (legend-entry 'i "sh") (legend-entry 'o "uo") (legend-entry 'p "ou")
    (legend-entry 'a "a") (legend-entry 's "ai") (legend-entry 'd "ao") (legend-entry 'f "an") (legend-entry 'g "ang")
    (legend-entry 'h "uang") (legend-entry 'j "ian") (legend-entry 'k "iao") (legend-entry 'l "in")
    (legend-entry 'z "un") (legend-entry 'x "ve/uai") (legend-entry 'c "uan") (legend-entry 'v "zh/ui") (legend-entry 'b "ia/ua")
    (legend-entry 'n "iu") (legend-entry 'm "ie"))
   (legend-layer 'st
    (legend-entry 'q "er") (legend-entry 'w "ei") (legend-entry 'e "e") (legend-entry 'r "en") (legend-entry 't "eng")
    (legend-entry 'y "ong") (legend-entry 'u "ch") (legend-entry 'i "sh") (legend-entry 'o "uo") (legend-entry 'p "ou")
    (legend-entry 'a "zh") (legend-entry 's "ai") (legend-entry 'd "ao") (legend-entry 'f "an") (legend-entry 'g "ang")
    (legend-entry 'h "uang") (legend-entry 'j "ian") (legend-entry 'k "iao") (legend-entry 'l "in")
    (legend-entry 'z "un") (legend-entry 'x "v/uai") (legend-entry 'c "uan") (legend-entry 'v "ui/ue") (legend-entry 'b "ia/ua")
    (legend-entry 'n "iu") (legend-entry 'm "ie"))
   (legend-layer 'jyutping
    (legend-entry 'q "—") (legend-entry 'w "w") (legend-entry 'e "e/eo") (legend-entry 'r "—") (legend-entry 't "t") (legend-entry 'y "yu")
    (legend-entry 'u "u/yun") (legend-entry 'i "i") (legend-entry 'o "o/oe") (legend-entry 'p "p")
    (legend-entry 'a "aa/a") (legend-entry 's "s") (legend-entry 'd "d") (legend-entry 'f "f") (legend-entry 'g "g/gw")
    (legend-entry 'h "h") (legend-entry 'j "j") (legend-entry 'k "k/kw") (legend-entry 'l "l")
    (legend-entry 'z "z") (legend-entry 'x "—") (legend-entry 'c "c") (legend-entry 'v "—") (legend-entry 'b "b")
    (legend-entry 'n "n/ng") (legend-entry 'm "m/ng"))))

(define (keyboard-legend-definition-ref layer [default #f])
  (define layer-symbol
    (cond
      [(symbol? layer) layer]
      [(string? layer) (string->symbol layer)]
      [else layer]))
  (hash-ref keyboard-legend-definitions layer-symbol (lambda () default)))

(define (keyboard-legend-text layer key [default ""])
  (define table (keyboard-legend-definition-ref layer))
  (if table
      (hash-ref table key (lambda () default))
      default))

(define keymap-definitions keyboard-legend-definitions)
(define keymap-ref keyboard-legend-definition-ref)
(define keymap-text keyboard-legend-text)
