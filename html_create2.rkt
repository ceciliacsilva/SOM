#lang racket

(require web-server/templates)
(require web-server/http)

(provide (all-defined-out))

(define (make_html name_simul_all cluster_data_group_order w colors filename)

  (call-with-output-file (string-append name_simul_all filename)
    #:exists 'replace
    (lambda(p)
      (displayln (string-replace (include-template "html/index-map.html") "\n" " ") p) ))
  )