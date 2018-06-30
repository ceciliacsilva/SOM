#lang racket

(require plot)
(plot-new-window? #t)

(require "functions.rkt")

(provide (all-defined-out))

(define (network_som input_list n_output_y n_output_x radius_max n_iterations_calc)
  (let ( (w_begin (weights_begin input_list n_output_y n_output_x))
         (lo 0.1)
         (precision 1e-10)
         (dist radius_max) )
    ;;(displayln (~a "w_begin: " w_begin))

    (let loop ( (w_prev_iter w_begin)
                (t 0) )

      (define-values (w_new)
        (for/fold ( (w_prev w_prev_iter) )
                  ( (input_line (in-list input_list)) )
          
          (define Dj_list
            (Dj_calc w_prev input_line) )

          ;;(displayln (~a "Dj_list: "  Dj_list))

          (define j_min (find_cluster Dj_list))

          ;;(displayln (~a "j_min: " j_min))

          (define learning_rate (learning_rate_time lo t n_iterations_calc radius_max))

          (define theta_dist (learning_rate_dist dist t radius_max n_iterations_calc))

          (define n_neighbour (exact-round (floor (neighbourhood_radius t radius_max n_iterations_calc))))

          (define w_new (neighbour_update_square w_prev input_line j_min learning_rate theta_dist n_neighbour n_output_y n_output_x))
        
          (values w_new)
          )
        )
      
      (define error_list (map (lambda(a b) (abs (- a b))) (flatten w_prev_iter) (flatten w_new)))

      (define stop? (for/and ( (i (in-list error_list)) )
                      (not (> i precision)) ) )
      (cond ( stop?
              (displayln (~a "Cycles: " t))
              
              w_new)
            (else
              (loop w_new (+ t 1)) ))
      
      );;end loop
    )
  )

(define (neighbour_update_square w_prev input_line j_min learning_rate theta_dist n_neighbour n_output_y n_output_x)

  (define j_min_y (car j_min))
  (define j_min_x (cadr j_min))
  
  (define-values (w_left w_toUpdate w_right)
    (neighbour_split w_prev j_min_y n_neighbour n_output_y))

  ;;(displayln w_left)
  ;;(displayln w_toUpdate)
  ;;(displayln w_right)

  (define w_new
    (append w_left
            (for/list ( (w_toUpdate_line (in-list w_toUpdate)) )
              (neighbour_update_square_line w_toUpdate_line input_line j_min_x n_neighbour learning_rate theta_dist n_output_x))
            w_right))
  
  w_new
  )

(define (neighbour_update_square_line w_line input_line j_min n_neighbour learning_rate theta_dist n_output_x)
  (define-values (w_left w_toUpdate w_right)
    (neighbour_split w_line j_min n_neighbour n_output_x))

  (define w_updated (weights_update w_toUpdate input_line learning_rate theta_dist))
  
  (define w_new (append w_left w_updated w_right))

  w_new
  )

(define (neighbour_split w_prev j_min n_neighbour n_output)
  (define n_left  (Some (- j_min n_neighbour) 0 n_output))
  (define n_right (Some (+ j_min n_neighbour 1) 0 n_output))
        
  (define w_toUpdate (take (drop w_prev n_left) (- n_right n_left)))

  (define w_left (take w_prev n_left))
  (define w_right (drop w_prev (Some n_right 0 n_output)))

  (values w_left w_toUpdate w_right)
)

(define (weights_update w_toUpdate input_line learning_rate theta_dist)
  (for/list ( (w_old (in-list w_toUpdate)) )
    (define w_new (map (lambda (xi wij) (+ wij (* learning_rate theta_dist (- xi wij))))
                       input_line w_old))
    
    w_new
    )
  )

(define (find_cluster Dj_list)
  (define-values (Dj_list_min Dj_list_index)
    (for/lists (mins index)
      ( (Dj_line (in-list Dj_list)) )
      (define min_line (apply min Dj_line))
      (define min_line_index (index-where Dj_line (=/c min_line)))
      (values min_line min_line_index))
    )
  ;;(displayln Dj_list_min)
  ;;(displayln Dj_list_index)
  
  (define Dj_min_index (index-where Dj_list_min (=/c (apply min Dj_list_min))))

  ;;(displayln Dj_min_index)
  
  (list Dj_min_index (list-ref Dj_list_index Dj_min_index))
  )

  


(define (Dj_calc w_prev input_line)
  (for/list ( (wk (in-list w_prev)) ) 
    (for/list ( (wj (in-list wk)) )
      (define Dj
        (for/sum ( (xi  (in-list input_line))
                   (wij (in-list wj))
                   (i (in-naturals)) )
          (sqr (- wij xi))
          )) 
      Dj)
    )
  )

(define (weights_begin input_list n_output_y n_output_x)
  (define input_transpose (transpose input_list))
  (define input_min_list (map (lambda(a) (apply min a)) input_transpose))
  (define input_max_list (map (lambda(a) (apply max a)) input_transpose))

  ;;(displayln input_transpose)
  ;;(displayln input_max_list)

  (for/list ( (j (in-range n_output_y)) )
    (for/list ( (i (in-range n_output_x)) )
      (for/list ( (input_min (in-list input_min_list))
                  (input_max (in-list input_max_list)) )
        ;;(if (= input_max 1) (random)
        ;;    (random (exact-round input_min)
        ;;            (exact-round input_max)))

        (* (random) input_max)
        
        )
      )
    )
  )

(define (neighbourhood_radius i radius_max n_iterations_max)
  (let* ( (time_lambda (time_constant n_iterations_max radius_max))
          (radius (* radius_max (exp (- (/ i
                                           time_lambda))) ))  )
    radius))
         
(define (learning_rate_time lo i n_iterations_max radius_max)
   (let* ( (time_lambda (time_constant n_iterations_max radius_max)) )
     (* lo (exp (- (/ i
                      time_lambda))))
     )  )

(define (learning_rate_dist dist i radius_max n_iterations_max)
  (let ( (tetha (neighbourhood_radius i radius_max n_iterations_max)) )
    (exp (- (/ (sqr dist)
            (* 2 (sqr tetha))) ))
    )
  )

(define (time_constant n_iterations_max radius_max)
  (/ n_iterations_max
     (log10 radius_max))
  )

(define (Some x x_min x_max)
  (cond ( (< x x_min) x_min)
        ( (> x x_max) x_max)
        ( else x)))

