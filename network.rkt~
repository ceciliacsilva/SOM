#lang racket

(require plot)
(plot-new-window? #t)

(define (network_som input_list n_output_x radius_max n_iterations_max [n_output_y 1])
  (let ( (w_begin (weights_begin input_list n_output_x))
         (lo 0.1)
         (precision 1e-10)
         (dist radius_max) )
    (let loop ( (w_prev_iter w_begin)
                (t 0) )

      ;;(displayln (~a "w_begin: " w_prev_iter))

      (define-values (w_new)
                         
        (for/fold ( (w_prev w_prev_iter) )
                  ( (input_line (in-list input_list)) )
                          
          (define Dj_list
            (Dj_calc w_prev input_line)
            )

          ;;          (displayln (~a "Dj_list: " Dj_list))
        
          (define j_min (find_cluster Dj_list) )

          ;;          (displayln (~a "j_min: " j_min))
        
          (define n_neighbour (exact-round (floor (neighbourhood_radius t radius_max n_iterations_max))))
        
          (define n_left  (Some (- j_min n_neighbour) 0 n_output_x))
          (define n_right (Some (+ j_min n_neighbour 1) 0 n_output_x))
        
          (define w_toUpdate (take (drop w_prev n_left) (- n_right n_left)))
        
          ;;                (displayln (~a "w_toUpdate: " w_toUpdate))

          (define learning_rate (learning_rate_time lo t n_iterations_max radius_max))

          (define theta_dist (learning_rate_dist dist t radius_max n_iterations_max))
        
          (define w_left (take w_prev n_left))
          (define w_right (drop w_prev (Some n_right 0 n_output_x)))
        
          (define w_updated (weights_update w_toUpdate input_line learning_rate theta_dist))
        
          (define w_new (append w_left w_updated w_right))

          ;;              (displayln (~a "w_new: " w_new))

          (values w_new)
                       
          )  )

      (define error_list (map (lambda(a b) (abs (- a b))) (flatten w_prev_iter) (flatten w_new)))

      (define stop? (for/and ( (i (in-list error_list)) )
                      (not (> i precision)) ) )

      (cond ( stop?
              (displayln (~a "iters: " t))
              (displayln (~a "w: " w_new))
              
              (define x-min (* 0.9 (apply min (map car w_new))))
              (define x-max (* 1.1 (apply max (map car w_new))))
              
              (define y-min (* 0.9 (apply min (map cadr w_new))))
              (define y-max (* 1.1 (apply max (map cadr w_new))))
              
              (plot
               (points w_new  #:sym 'fullcircle2
                       #:color "blue")
               #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
               )
              #|(lambda (input_line)
              (define Dj_list
                (Dj_calc w_new input_line) )
       
              (define j_min (find_cluster Dj_list) )

              (displayln (~a "Cluster: " j_min))
              (displayln (~a "Value: " (list-ref Dj_list j_min)))
              )|#
              )
            (else 
             (loop w_new (+ t 1)) )
            )
      
      )
    )
  )

(define (1divx_fun max)
    (for/list ( (x (range 1 max 0.001)) )
      (list x (exact->inexact(/ 1 x)))))

(define 1divx (shuffle (1divx_fun 10)))

(define (Dj_calc w_prev input_line)
  (for/list ( (wj (in-list w_prev)) )
    (define Dj
      (for/sum ( (xi (in-list input_line))
                 (i (in-naturals)) )
        (let ( (wij (list-ref wj i)) )
          (sqr (- wij xi))
          )) )
    Dj) )

(define (find_cluster Dj_list)
  (index-where Dj_list (=/c (apply min Dj_list))))

(define (weights_update w_toUpdate input_line learning_rate theta_dist)
  (for/list ( (w_old (in-list w_toUpdate)) )
    (define w_new (map (lambda (xi wij) (+ wij (* learning_rate theta_dist (- xi wij))))
                       input_line w_old))
    
    w_new
    )
  )

(define (Some x x_min x_max)
  (cond ( (< x x_min) x_min)
        ( (> x x_max) x_max)
        ( else x)))

(define (weights_begin input_list n_output_x)
  (define input_transpose (transpose input_list))
  (define input_min_list (map (lambda(a) (apply min a)) input_transpose))
  (define input_max_list (map (lambda(a) (apply max a)) input_transpose))

  (for/list ( (i (in-range n_output_x)) )
    (for/list ( (input_min (in-list input_min_list))
                (input_max (in-list input_max_list)) )
      (random (exact-round input_min)
              (exact-round input_max)))
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

(define (transpose xss)
  (apply map list xss))

(define (log10 x)
  (/ (log x)
     (log 10))
  )