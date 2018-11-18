
; coded 11/13/78 by sas
; modified 3/6/79 by sas to work better
; modified 5/17/79 by sas for ortho commands

(documentation ">doc>sm>dcl_mode.doc")

(static dcl_buffer tg0 tg1 tg2 tm0 declared_flag)
(variable print_msg read_line whitespace)
(variable current_buffer keep_on_tunneling M_dispatch O_dispatch)
(variable start_of_statement next_pl1_token)

(defun dcl_mode()
       (bind_key M-c do_call_dcl)
       (bind_key O-e dcl_elsewhere)
       (bind_key O-d do_declare)
       (bind_key O-c do_call)
       (keep_on_tunneling))

(defun user_init()
       (store (make_mark) tm0)
       (store (make_gnirt) tg0)
       (store (make_gnirt) tg1)
       (store (make_gnirt) tg2)
       (store (make_buffer) dcl_buffer)
       (read_dcl_file))

(defun read_dcl_file()
       (insert ">u>ss>" tg1)
       (insert (call_af "user id") tg1)
       (insert ".dcls" tg1)

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
       (delete tg1 99999)
       (store (location current_buffer) here)
       (set_loc current_buffer (find_first_not_in_ba current_buffer "      
)(,+-*/=^&|[]"))
       (store (location current_buffer) end)
       (set_loc current_buffer (find_first_in_ba current_buffer "     
)(,+-*/=^&|[]"))
       (insert_region current_buffer (location current_buffer) end tg1)
       (set_loc current_buffer here))


(defun find_dcl(vbl &aux start)
       (delete tg2 99999)
       (cat_into tg0 "dcl " vbl " ")
       (set_loc dcl_buffer 0)
       (set_loc dcl_buffer  (iferror (search dcl_buffer tg0)
                                     (progn (print_msg "Not known." 1)
                                            (goto exit))))
       (store (location dcl_buffer) start)
       (set_loc dcl_buffer (iferror (search dcl_buffer ";")
                                    (length dcl_buffer)))
       (insert_region dcl_buffer start (location dcl_buffer) tg2)
exit   )


(defun dcl_elsewhere()
       (snarf_token)
       (declaredp tg1)
       (cond (declared_flag
              (print_msg "Already dcld." 1)
              (goto punt)))
       (find_dcl tg1)
       (cond ((eq (length tg2) 0)
              (cat_into tg2 (read_line "Declaration:" 13) ";")))
       (insert_dcl tg1 tg2)
punt   )



(defun do_declare()
       (snarf_token)
       (find_dcl tg1)
       (insert " " current_buffer)
       (insert tg2 current_buffer))


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





(defun declaredp(vbl &aux oldpos)
       (store (t) declared_flag)
       (cat_into tg0 " dcl " vbl 32)
       (store (location current_buffer) oldpos)
       (set_loc current_buffer
                (iferror (rsearch current_buffer "/* declarations */")
                         (add 0 0)))
       (set_loc current_buffer (iferror (search current_buffer tg0)
                                        (goto no)))
       (ift (gp (location current_buffer) oldpos) no)
       (set_loc current_buffer oldpos)
       (return)
no     (set_loc current_buffer oldpos)
       (store (nil) declared_flag)
exit)




(defun insert_dcl(vbl dcl)
       (set_mark tm0 current_buffer (location current_buffer))
       (set_loc current_buffer
                (iferror (rsearch current_buffer "/* declarations */")
                         (add 0 0)))
       (set_loc current_buffer
                (iferror (search current_buffer 13) (length current_buffer)))

       (insert " dcl " current_buffer)
       (insert vbl current_buffer)
       (insert 32 current_buffer)
       (insert dcl current_buffer)
       (insert 13 current_buffer)
       (set_loc current_buffer (eval_mark tm0 current_buffer))
exit   )
^L