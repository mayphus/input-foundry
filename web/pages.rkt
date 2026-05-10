#lang racket/base

(require xml
         "locale.rkt"
         "components.rkt")

(provide render-page
         render-exhibit-page)

(define (catalog-page req schemas layouts)
  (define locale (request-locale req))
  (page-xexpr
   locale
   "/"
   `((section ((class "rime-hero-card"))
              (div ((class "rime-hero-head"))
                   (div
                    (h1 ((class "page-title")) ,(t locale 'title)))))
     ,@(catalog-filter locale)
     (div ((class "rime-schema-catalogs"))
          ,@(for/list ([catalog (in-list (cataloged-schemas schemas))])
              (catalog-section locale layouts catalog))))))

(define (exhibit-page req schemas layouts schema-id*)
  (define locale (request-locale req))
  (define requested-schema (schema-by-id schemas schema-id*))
  (define schema requested-schema)
  (define platform (request-value req "platform" "mobile"))
  (define artifact
    (cond
      [(equal? platform "desktop") "rime"]
      [else "yuanshu"]))
  (define current-path (format "/exhibits/~a" (if schema (schema-id schema) schema-id*)))
  (page-xexpr
   locale
   current-path
   (if schema
       (let ([artifacts (schema-artifacts schema)])
         `((section ((class "rime-exhibit-overview"))
                    (div ((class "rime-exhibit-copy"))
                         (a ((class "rime-back-link") (href "/")) ,(t locale 'back))
                         (h1 ((class "page-title")) ,(schema-name locale schema))
                         (p ((class "rime-section-copy rime-hero-copy"))
                            ,(schema-description locale schema)))
                    ,@(let ([preview (schema-detail-preview locale schema layouts)])
                        (if preview (list preview) '()))
                    (div ((class "rime-exhibit-download"))
                         ,(artifact-form locale
                                         schema
                                         (list schema)
                                         (if (member artifact artifacts)
                                             (list artifact)
                                             artifacts)
                                         layouts)))))
       `((section ((class "rime-hero-card"))
                  (a ((class "rime-back-link") (href "/")) ,(t locale 'back))
                  (h1 ((class "page-title")) ,(t locale 'missing)))))))

(define (render-page req schemas layouts #:route [_route 'home])
  (xexpr->string (catalog-page req schemas layouts)))

(define (render-exhibit-page req schemas layouts schema-id)
  (xexpr->string (exhibit-page req schemas layouts schema-id)))
