#lang racket/base

(require rackunit
         racket/file
         "../build.rkt")

(define (check-upload-skin-files tmp skin)
  (check-true (file-exists? (build-path tmp skin "config.yaml")))
  (check-true (file-exists? (build-path tmp skin "README.md")))
  (check-true (file-exists? (build-path tmp skin "demo.png")))
  (check-true (file-exists? (build-path tmp skin "demo.svg")))
  (check-true (file-exists? (build-path tmp skin "light" "pinyinPortrait.yaml")))
  (check-true (file-exists? (build-path tmp skin "dark" "pinyinPortrait.yaml"))))

(module+ test
  (test-case "profiles build unpacked skin directories for direct Yuanshu upload"
    (define tmp (make-temporary-file "rime-config-skins-~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define skins
          (build-profile-skin-directories!
           (hash 'schemas (list "flypy") 'desktop? #f)
           "test"
           tmp))
        (check-equal? skins '("flypy"))
        (check-upload-skin-files tmp "flypy")
        (check-false (file-exists? (build-path tmp "flypy.cskin"))))
      (lambda ()
        (delete-directory/files tmp #:must-exist? #f))))

  (test-case "all generated upload skins include demo assets"
    (define tmp (make-temporary-file "rime-config-all-skins-~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define skins
          (build-profile-skin-directories!
           (hash 'schemas "all" 'desktop? #f 'skip-default-custom #t)
           "test-all"
           tmp))
        (check-not-equal? skins '())
        (for ([skin (in-list skins)])
          (check-upload-skin-files tmp skin)))
      (lambda ()
        (delete-directory/files tmp #:must-exist? #f)))))
