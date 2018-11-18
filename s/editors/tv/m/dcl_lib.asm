
(documentation ">doc>sm>dcl_lib.doc")

; coded 11/13/78 by sas
; modified 3/6/79 by sas to work better

(static dcl_buffer tg1)
(variable print_msg read_line whitespace)
(variable current_buffer dispatch)
(variable start_of_statement next_pl1_token)

(defun user_init()
       (as 'do_call_dcl dispatch 3)          ; C-c
       (store (make_gnirt) tg1)
       (insert ">u>ss>" tg1)
       (insert (call_af "user id") tg1)
       (insert ".dcls" tg1)
       (store (make_buffer) dcl_buffer)

       (cond ((gep (read_file dcl_buffer tg1) 0)
              (t))
             ((gep (read_file dcl_buffer ">u>ss>default.dcls") 0)
              (t))
             ((t)
              (print_msg "Cannot find dcls." 1))))


(defun do_call_dcl(&aux save start)
       (store (location current_buffer) save)

       (start_of_statement)
       (store (location current_buffer) start)
       (next_pl1_token)
       (delete tg1 99999)
       (snarf_token)

       (set_loc current_buffer save)
       (cond ((or (eq tg1 "dcl")
                  (eq tg1 "declare"))
              (do_declare))
             ((t)
              (do_call))))


(defun snarf_token(&aux here end)
       (store (location current_buffer) here)
       (set_loc current_buffer (find_first_not_in_ba current_buffer "      
"))
       (store (location current_buffer) end)
       (set_loc current_buffer (find_first_in_ba current_buffer "     
"))
       (insert_region current_buffer (location current_buffer) end tg1)
       (set_loc current_buffer here))




(defun do_declare(&aux start)
       (delete tg1 99999)
       (insert "dcl " tg1)
       (snarf_token)
       (insert " " tg1)

       (set_loc dcl_buffer 0)
       (set_loc dcl_buffer  (iferror (search dcl_buffer tg1)
                                     (progn (print_msg "Not known." 1)
                                            (goto exit))))
       (store (location dcl_buffer) start)
       (set_loc dcl_buffer (iferror (search dcl_buffer ";")
                                    (length dcl_buffer)))
       (insert " " current_buffer)
       (insert_region dcl_buffer start (location dcl_buffer)
                      current_buffer)
exit   )


(defun do_call(&aux start)
       (delete tg1 99999)
       (insert "call " tg1)
       (snarf_token)
       (insert 13 tg1)

       (set_loc dcl_buffer 0)
       (set_loc dcl_buffer  (iferror (search dcl_buffer tg1)
                                     (progn (print_msg "Not known." 1)
                                            (goto exit))))

       (insert "(" current_buffer)
loop   (store (location dcl_buffer) start)
       (ift (eq (nthr dcl_buffer 0) 13) done)
       (set_loc dcl_buffer (iferror (search dcl_buffer 13)
                                 (length dcl_buffer)))
       (delete tg1 99999)
       (insert_region dcl_buffer start (sub (location dcl_buffer) 1) tg1)
       (insert ": " tg1)
       (insert (read_line tg1 13) current_buffer)
       (insert "," current_buffer)
       (goto loop)
done   (ift (eq (nthr current_buffer -1) "(") fini)
       (delete current_buffer -1)
fini   (insert ")" current_buffer))

^L