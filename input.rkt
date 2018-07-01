#lang racket

(require "functions.rkt")

(define *data_empty* "-")

(provide (all-defined-out))

(define (create_input_train [file_data "data.csv"] [file_names "names.csv"])
  (define file_data_all (string-append "data/" file_data))
  (define file_names_all (string-append "data/" file_names))
  
  (define-values (names_list data_list) (read_input file_data_all file_names_all))

  (define data_list_transpose (transpose data_list))

  (define data_toTrain_list
    (for/list ( (name (in-list names_list))
                (data (in-list data_list_transpose)) )
      (cons name
            (map (lambda(a)
                   (define n (string->number a))
                   (if n
                       (list n)
                       a))
                 (remove* (list *data_empty*)
                          data))) )
    )
  (values
   data_list
   data_toTrain_list)
  )

(define (data_list_get_topic topics data_list)
  (let loop ( (data_list_iter data_list)
              (accTrain '())
              (accTest '()) )
    
    (cond ( (null? data_list_iter) (values accTrain accTest) )
          ( else
            (define values
              (for/list ( (topic (in-list topics)) )
                (string->number
                 (list-ref (car data_list_iter) topic)) ))

            (define values_train (remove* (list #f) values))
            (define values_test (map (lambda(a) (if a
                                                    a
                                                    0.0)) values))

            (define name (caar data_list_iter))
            
            (loop (cdr data_list_iter)
                  (if (= (length values_train)
                         (length values_test))
                      (cons values_train accTrain)
                      accTrain)
                  (cons (cons name values_test) accTest)) )  )
    )
  )

(define (read_input  file_data file_names)
  (define data_file file_data)
  (define name_file file_names)

  (define names_control
    (call-with-input-file name_file
      (lambda(p)
        (split_line (read-line p)))) )
  
  (define data_control
    (call-with-input-file data_file
      (lambda(p)
        (for/list ( (file_line (in-lines p)) )
          (split_line file_line)
          )))
    )
  (values names_control
          data_control)
  )

(define (split_line txt)
  (let ( (txt_split (regexp-split "," txt)) )
    txt_split
    )
  )
