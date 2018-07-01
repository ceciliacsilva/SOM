#lang racket

(require pict)

(provide (all-defined-out))

(require "network-nxn.rkt")
(require "functions.rkt")

(define (draw_txt txt [txt_size 25] [txt_color "tomato"])
  (colorize (text txt null txt_size) txt_color)
  )

(define (draw_txt_list txt_all_list)
  (map (lambda(a)
         (apply draw_txt a)) txt_all_list)
  )

(define (draw_rectangle width height color)
  ;;(if (equal? color '(255 255 255))
  ;;    (rectangle width height)
  (lt-superimpose
   (colorize (filled-rectangle width height) color)
   (rectangle width height)
   )
  ;;    )
  )

(define (draw_rectangle_list w_list)
  (let ( (width 170)
         (height 100) )

    (define w_list_avg (map (lambda(b) (map (lambda(a) (list_average a)) b)) w_list))
    ;;(define w_list_avg (map (lambda(a) (list_average a)) w_list))
    
    (define w_max (apply max (flatten w_list_avg)))
    (define w_min (apply min (flatten w_list_avg)))
    
    (for/list ( (w_line (in-list w_list)) )
      (for/list ( (w (in-list w_line)) )
        (define w_value
          (list_average w)
          )
        

        (define w_normalized (+ 0.2 (* 0.8 (/ (- w_value w_min) (- w_max w_min)))))
        
        (define color (make-list 3 (exact-round (* w_normalized 255))))
        
        (draw_rectangle width height color)
        )
      )
    )
  )

(define (draw_graphic w_list input_list)
  ;; input_list element: '(name . (data))

  (define rectangles (draw_rectangle_list w_list))

  (define input_cluster 
    (for/list ( (input_element (in-list input_list)) )
      (match input_element
        ( (cons name data)
          (define c (find_cluster (Dj_calc w_list data)))
          
          (cons name c) )
        ))
    )

 (apply vc-append
        (for/list ( (rect_line (in-list rectangles))
                    (i (in-naturals)) )
          (apply hc-append
                 (for/list ( (rect (in-list rect_line))
                             (j (in-naturals)) )
             
                   (define txt_color (list (random 50 200)
                                           (random 50 200)
                                           (random 50 200)) )
             
                   (define filter_cluster
                     (lambda(a) (and (equal? (cdr a) (list i j))
                                     (list (car a)
                                           20
                                           txt_color)
                                     )) )
             
                   (define names
                     (filter-map filter_cluster input_cluster))
             
                   (define names_txt
                     (draw_txt_list names))

                   (define names_txt_splited
                    (group-by (lambda (x) (modulo (index-of names_txt x) 3)) names_txt))

                                
                   (define names_pict
                     (apply vc-append (append (list 10) (map (lambda(x) (apply hc-append (append (list 10) x))) names_txt_splited))))
             
                   (if (null? names_pict)
                       rect
                       (ct-superimpose rect names_pict))
                   )
                 );;end hc-append
          )
        );;end vc-append
  )

(define (save_pict the_pict name kind)
  (define bm (pict->bitmap the_pict))
  (send bm save-file (string-append name "." (symbol->string kind)) kind))