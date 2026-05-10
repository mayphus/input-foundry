#lang racket/base

(require racket/list
         racket/string
         "../input-method/registry.rkt"
         "locale.rkt")

(provide schema-id
         schema-name
         schema-description
         schema-artifacts
         schema-by-id
         schema-layout-items
         schema-detail-preview
         cataloged-schemas
         page-xexpr
         catalog-section
         artifact-form
         layout-detail-card)

(define (schema-id schema)
  (hash-ref schema 'id))

(define (schema-name locale schema)
  (localized-value (hash-ref schema 'names
                             (hash-ref schema 'name (schema-id schema)))
                   locale
                   (schema-id schema)))

(define (schema-description locale schema)
  (localized-value (hash-ref schema 'descriptions
                             (hash-ref schema 'description ""))
                   locale))

(define (schema-artifacts schema)
  (hash-ref schema 'artifacts '()))

(define (schema-keyboard-layouts schema)
  (hash-ref schema 'keyboard-layouts '()))

(define (schema-by-id schemas id)
  (for/first ([schema (in-list schemas)]
              #:when (equal? id (schema-id schema)))
    schema))

(define (layout-id layout)
  (hash-ref layout 'id))

(define (layout-name locale layout)
  (localized-value (hash-ref layout 'names
                             (hash-ref layout 'name (layout-id layout)))
                   locale
                   (layout-id layout)))

(define (layout-by-id layouts id)
  (for/first ([layout (in-list layouts)]
              #:when (equal? id (layout-id layout)))
    layout))

(define (schema-layout-items schema layouts)
  (filter values
          (for/list ([layout-id (in-list (schema-keyboard-layouts schema))])
            (layout-by-id layouts layout-id))))

(define (cataloged-schemas schemas)
  (filter-map
   (lambda (catalog-id)
     (define items
       (filter (lambda (schema)
                 (equal? (schema-id->catalog-id (schema-id schema)) catalog-id))
               schemas))
     (and (pair? items) (cons catalog-id items)))
   schema-catalog-order))

(define (classes . parts)
  (string-join (filter (lambda (part) part) parts) " "))

(define (language-toggle locale current-path)
  `(a ((class "rime-language-toggle rime-footer-language")
       (href ,(format "~a?locale=~a"
                      current-path
                      (symbol->string (next-locale locale)))))
      ,(t locale 'language)))

(define (layout-preview locale layout #:base-path [base-path "layouts"])
  (preview-image (format "/~a/~a/preview.svg" base-path (layout-id layout))
                 (format "/~a/~a/preview-dark.svg" base-path (layout-id layout))
                 (layout-name locale layout)))

(define (preview-image light-path dark-path alt-text #:class [extra-class #f])
  `(div ((class ,(classes "rime-layout-preview"
                          extra-class
                          "keyboard-preview"
                          "keyboard-preview-svg-wrap")))
        (picture
         (source ((media "(prefers-color-scheme: dark)")
                  (srcset ,(format "~a?v=~a" dark-path preview-svg-version))))
         (img ((class "keyboard-preview-svg")
               (loading "lazy")
               (src ,(format "~a?v=~a" light-path preview-svg-version))
               (alt ,alt-text))))))

(define (schema-platforms schema)
  (define artifacts (schema-artifacts schema))
  (filter values
          (list (and (member "rime" artifacts) "desktop")
                (and (member "yuanshu" artifacts) "mobile"))))

(define (schema-card-preview locale schema #:platform [platform #f])
  (define name (schema-name locale schema))
  (define artifacts (schema-artifacts schema))
  (define has-yuanshu? (member "yuanshu" artifacts))
  (cond
    [(equal? platform "desktop")
     (preview-image (format "/schemas/~a/preview.svg" (schema-id schema))
                    (format "/schemas/~a/preview-dark.svg" (schema-id schema))
                    name)]
    [(equal? platform "mobile")
     (preview-image (format "/schemas/~a/skin-preview.svg" (schema-id schema))
                    (format "/schemas/~a/skin-preview-dark.svg" (schema-id schema))
                    name)]
    [has-yuanshu?
     (preview-image (format "/schemas/~a/skin-preview.svg" (schema-id schema))
                    (format "/schemas/~a/skin-preview-dark.svg" (schema-id schema))
                    name)]
    [else
     (preview-image (format "/schemas/~a/preview.svg" (schema-id schema))
                    (format "/schemas/~a/preview-dark.svg" (schema-id schema))
                    name)]))

(define (schema-detail-preview locale schema layouts)
  (define preview-layouts (schema-layout-items schema layouts))
  (and (pair? preview-layouts)
       `(div ((class "rime-detail-preview"))
             ,(preview-image (format "/schemas/~a/skin-preview.svg" (schema-id schema))
                             (format "/schemas/~a/skin-preview-dark.svg" (schema-id schema))
                             (schema-name locale schema)))))

(define (schema-card locale schema layouts #:platform [platform #f])
  (define preview-layouts (schema-layout-items schema layouts))
  `(a ((class "rime-exhibit-card")
       (href ,(format "/exhibits/~a~a"
                      (schema-id schema)
                      (if platform (format "?platform=~a" platform) ""))))
      (div ((class "rime-option-head"))
           (div ((class "rime-option-copy"))
                (div ((class "rime-option-title-row"))
                     (span ((class "rime-option-title")) ,(schema-name locale schema)))))
      ,@(if (pair? preview-layouts)
            `((div ((class "rime-schema-previews"))
                   ,(schema-card-preview locale schema #:platform platform)))
            '())))

(define (schema-cards locale schema layouts)
  (define platforms (schema-platforms schema))
  (if (> (length platforms) 1)
      (for/list ([platform (in-list platforms)])
        (schema-card locale schema layouts #:platform platform))
      (list (schema-card locale schema layouts))))

(define (catalog-section locale layouts catalog)
  (define catalog-id (car catalog))
  (define schemas (cdr catalog))
  `(section ((class "rime-schema-catalog"))
            (div ((class "rime-catalog-heading"))
                 (h2 ((class "rime-schema-catalog-title"))
                     ,(schema-catalog-label catalog-id locale)))
            (div ((class "rime-option-grid"))
                 ,@(append-map (lambda (schema)
                                  (schema-cards locale schema layouts))
                                schemas))))

(define (footer locale current-path)
  `(footer ((class "rime-footer"))
           (span ((class "rime-footer-credit")) ,(t locale 'footer-credit))
           (div ((class "rime-footer-support"))
                (span ((class "rime-footer-support-label")) ,(t locale 'support))
                (img ((class "rime-footer-support-image")
                      (src "/support-8f6d2b.svg")
                      (alt ,(t locale 'support)))))
           ,(language-toggle locale current-path)))

(define dev-reload-script
  `(script
    ((type "module"))
    "const url='/__dev/reload-token';
let token=null;
async function check(){
  try {
    const res=await fetch(url,{cache:'no-store'});
    if(!res.ok) return;
    const next=(await res.text()).trim();
    if(token===null) token=next;
    else if(next && next!==token) location.reload();
  } catch (_) {}
}
setInterval(check, 700);
check();"))

(define (page-xexpr locale current-path body)
  `(html ((lang ,(if (eq? locale 'zh-Hant) "zh-Hant" "en")))
         (head
          (meta ((charset "utf-8")))
          (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
          (title ,(t locale 'title))
          (link ((rel "stylesheet") (href ,app-css-href))))
         (body
          (main ((id "app"))
                (div ((class "rime-museum-shell"))
                     ,@body
                     ,(footer locale current-path)))
          ,@(if (getenv "INPUT_FOUNDRY_DEV_RELOAD")
                (list dev-reload-script)
                '()))))

(define (schema-select locale schema variants)
  (if (> (length variants) 1)
      `(label ((class "rime-variant-control"))
              (span ((class "rime-variant-label")) ,(t locale 'dictionary))
              (select ((class "rime-variant-select") (name "schemas"))
                      ,@(for/list ([variant (in-list variants)])
                          `(option ((value ,(schema-id variant))
                                    ,@(if (equal? (schema-id variant) (schema-id schema))
                                          '((selected "selected"))
                                          '()))
                                   ,(schema-name locale variant)))))
      `(input ((type "hidden") (name "schemas") (value ,(schema-id schema))))))

(define (artifact-button locale artifact)
  `(button ((class ,(classes "rime-build-button"
                             (and (equal? artifact "yuanshu")
                                  "rime-build-button-secondary")))
            (type "submit")
            (name "artifact")
            (value ,artifact))
           ,(t locale 'download)))

(define (artifact-action locale artifact)
  `(div ((class "rime-artifact-action"))
        ,(artifact-button locale artifact)))

(define (artifact-form locale schema variants artifacts layouts)
  `(form ((class "rime-artifact-form")
          (method "post")
          (action "/build"))
         ,(schema-select locale schema variants)
         (div ((class "rime-artifact-buttons"))
             ,@(for/list ([artifact (in-list artifacts)])
                  (artifact-action locale artifact)))))

(define (layout-detail-card locale layout)
  `(article ((class "rime-layout-card"))
            ,(layout-preview locale layout)))
