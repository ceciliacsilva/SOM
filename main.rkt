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
  
  (func  w data_test name_simul_all name)
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