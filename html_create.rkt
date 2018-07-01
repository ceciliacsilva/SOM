#lang racket

(require web-server/templates)
(require web-server/http)

(define (make_html name_simul)

  (define name_simul_all (string-append "trained/" name_simul))

  (define graphics_files (directory-list name_simul_all))

  (define graphics_images (filter-map (lambda(a) (regexp-match #rx"[a-zA-z0-9\\-]*.png" a)) graphics_files))

  (define graphics
    (map (lambda(a)
           (define name (car (regexp-split #rx"\\." (car a))))
           (list name
                 ;;(string-append name_simul_all (car a))
                 (car a)
                 ))
         graphics_images))

  ;;(displayln graphics)

  ;(define graphics (list (list "oi" "teste.png")
  ;                       (list "oi2" "teste.png")))

  (call-with-output-file (string-append name_simul_all "output.html")
    #:exists 'replace
    (lambda(p)
      (displayln (string-replace (include-template "html/index.html") "\n" " ") p) ))
  )