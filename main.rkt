#lang racket

(require plot)

(plot-new-window? #t)

(require "input.rkt")
(require "draw.rkt")
(require "network-nxn.rkt")
(require "functions.rkt")


(define (train_topics file_data file_names topics name n_output_y n_output_x radius_max n_iterations_calc name_simul [func func_w])
  (define name_simul_all (string-append "trained/" name_simul "/"))

  (make-directory* name_simul_all)
  
  (define-values
    (data_list data_toTrain_list) (create_input_train file_data file_names))

  (define-values (data_train data_test)
    (data_list_get_topic topics data_list))

  (define w (time (network_som data_train n_output_y n_output_x radius_max n_iterations_calc)))

  (write_w w name_simul_all name)
  
  (func w data_test name_simul_all name)
  )

(define (func_w  w data_test name_simul_all name)
  w
  )

(define (func_plot  w data_test name_simul_all name)
  (define picture
    (plot-pict
     #:width 2000
     #:height 1000
     (list
      (points (map (lambda(x) (take (cdr x) 2)) data_test) #:sym 'fullcircle1 #:color "blue")
      (map (lambda(x) (map (lambda(w_each) (points (list (take w_each 2)) #:sym 'fullcircle5 #:color "red")) x)) w)
      
      )
     #:x-label "Latitude" #:y-label "Longitude"
     )
    )

  (save_pict picture (string-append name_simul_all name) 'png)
  )

;;(define colors '( ((0 0) "red") ((0 1) "Medium Aquamarine") ((0 2) "seagreen") ((0 3) "purple")
;;                  ((0 4) "orange") ((0 5) "Black") ((0 6) "deepskyblue") ((0 7) "seagreen")
;;                  ((0 8) "magenta") ((0 9) "darkcyan") )  )

(define (make_colors n)
  (for/list ( (i (in-range n)) )
    (list
     (list 0 i)
     (list (random 256)
           (random 256)
           (random 256)) ))
  )

(define (func_plot_colors  w data_test_all name_simul_all name)

  (define data_test (remove-duplicates data_test_all))
  
  (define w_car_len (length (car w)))

  (define colors (make_colors w_car_len))
  
  (define clusters (map (lambda(a) (map (lambda(x) (find_cluster (Dj_calc w x))) a)) w))

  ;;(define cluster_order (sort clusters < #:key cadar))

  ;;(displayln clusters)

  ;;(displayln (car data_test))
  
  (define cluster_data
    (for/list ( (x_input (in-list data_test)) )
      (cons
       (find_cluster (Dj_calc w (cdr x_input)))
       (take (cdr x_input) 2)
       )
      ))

  ;;(displayln "oi")
  
  (define cluster_data_group
    (group-by (lambda(x)
                (index-of (map car colors) (car x))) cluster_data))

  (define cluster_data_group_order
    (sort cluster_data_group < #:key cadaar))

  (define maps_dir (string-append name_simul_all "maps/"))
  
  (make-directory* maps_dir)
  
  (for ( (x_all (in-list cluster_data_group_order))
         (w_each (in-list (car w)))
         (i (in-naturals)) )
    (call-with-output-file (~a maps_dir "cluster" i ".csv")
      #:exists 'replace
      (lambda (p)
        (displayln (~a "latitude,longitude,distancia_media,valor_medio_semana,volume_medio_semana,peso_medio_semana") p)

        (displayln (~a (car w_each) "," (cadr w_each)) p)
        
        (for ( (city_all (in-list x_all)) )
          (displayln (~a (cadr city_all) "," (caddr city_all)) p) )
        )
      ))
  
  (define picture
    (plot-pict
     #:width 2000
     #:height 1000
     (list
      ;;(map (lambda(x) (points (list (take (cdr x) 2)) #:color (cadr (assoc (find_cluster (Dj_calc w (cdr x))) colors)) #:sym 'fullcircle1 )) data_test)
      ;;(points (map (lambda(x) (take (cdr x) 2)) data_test) #:sym 'fullcircle1 #:color)
      ;;(map (lambda(x) (map (lambda(w_each) (points (list (take w_each 2)) #:sym 'fullcircle5 #:color "red")) x)) w)

      (for/list ( (x_all (in-list cluster_data_group_order))
                  (color (in-list colors)) )

        ;;(displayln (car x_all))
        ;;(define color (cadr (assoc (car x_all) colors)))
        (points (map cdr x_all) #:color (cadr color) #:sym 'fullcircle1))
                     
      (for/list ( (w_line (in-list w)) )
        (for/list ( (w_each (in-list w_line)) )
          (define cluster (find_cluster (Dj_calc w w_each)))
          (define color (cadr (assoc cluster colors)))
          
          (define pos (list (take w_each 2)))

          ;;(displayln (~a cluster "   " color "   " pos))

          (points pos #:color color #:sym 'fullcircle8)
          ))
          
      )
     #:x-label "Latitude" #:y-label "Longitude"
     )
    )

  (save_pict picture (string-append name_simul_all name) 'png)
  )

(define (func_save_pict w data_test name_simul_all name)
  (save_pict (draw_graphic w data_test) (string-append name_simul_all name) 'png)
  )

(define (train file_data file_names n_all n_output_y n_output_x radius_max n_iterations_calc name_simul)

  (define name_simul_all (string-append "trained/" name_simul "/"))

  (make-directory* name_simul_all)
  
  (define-values
    (data_list data_toTrain_list) (create_input_train file_data file_names))

  (define data_train_list (cdr data_toTrain_list))

  ;;data_train_list

  (for ( (each_data_train (in-list data_train_list))
         (i (in-naturals 1)) )

    ;;(displayln each_data_train)

    (define name_graph (car each_data_train))
    (define data_toRun (cdr each_data_train))
    
    (displayln (~a "Network - " name_graph))
    
    (define w (time (network_som (cdr each_data_train) n_output_y n_output_x radius_max n_iterations_calc)))
    
    (define toDraw_all
      (for/list ( (country (in-list (cdar data_toTrain_list))) )
        (define country_data (assoc country data_list))

        (define country_name (list-ref country_data 0))
        (define value (list-ref country_data i))

        (cons country_name
              (let ( (n (string->number value)) )
                (if n (list n) null)) )
        
        )
      )
    
    (define toDraw
      (remove*_pos null toDraw_all cdr)
      ;;toDraw_all
      )
    
    (write_w w name_simul_all name_graph)
    
    (save_pict (draw_graphic w toDraw) (string-append name_simul_all name_graph) 'png)
    )
  ;;(values w
  ;;        (draw_rectangle_list w)
  ;;        toDraw)

  (displayln (~a "Network - All"))
  (train_topics (create_list n_all) "All" n_output_y n_output_x radius_max n_iterations_calc name_simul)
  )

(define (write_w w name_simul_all name_graph)
  (call-with-output-file (string-append name_simul_all name_graph ".data")
    #:exists 'replace
      (lambda(p)
        (displayln w p)))
  )

(define (read_w filename)
  (define file_str (file->string filename))

  (call-with-input-string file_str
                          (lambda(p)
                            (read p)))
  )