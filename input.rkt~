#lang racket

(require "functions.rkt")

(define *data_empty* "-")

(provide (all-defined-out))

(define (create_input_train)
  (define-values (names_list data_list) (read_input))

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

(define (read_input)
  (define data_file "data/data.csv")
  (define name_file "data/names.csv")

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
