#lang s-exp "lib/lang.rkt"

(rime-schema bopomofo
  (name "注音")
  (static-files "terra_pinyin.dict.yaml" "zhuyin.yaml")
  (mobile-skin bopomofo
    (meta
      (name "Bopomofo" "注音")
      (summary "A Yuanshu skin for Bopomofo input with the standard secondary pages.")
      (features
        "Bopomofo phone layout"
        "Bundled custom iPad pages"))
    (phone-layout bopomofo)))
