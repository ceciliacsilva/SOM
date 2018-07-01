#lang racket

(require "input.rkt")
(require "draw.rkt")
(require "network-nxn.rkt")
(require "functions.rkt")


(define (train n_output_y n_output_x radius_max n_iterations_calc name_simul)

  (define name_simul_all (string-append "trained/" name_simul "/"))

  (make-directory* name_simul_all)
  
  (define-values
    (data_list data_toTrain_list) (create_input_train))

  (define data_train_list (cdr data_toTrain_list))

  ;;data_train_list

  (for ( (each_data_train (in-list data_train_list))
         (i (in-naturals 1)) )

    ;;(displayln each_data_train)
    
    (define w (network_som (cdr each_data_train) n_output_y n_output_x radius_max n_iterations_calc))
    
    (define name_graph (car each_data_train))
    (define data_toRun (cdr each_data_train))
  
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
  
  )

(define (write_w w name_simul_all name_graph)
  (call-with-output-file (string-append name_simul_all name_graph ".data")
    #:exists 'replace
      (lambda(p)
        (displayln w p)))
  )
