
(defun user_init ()
       (store (make_buffer) completion_table)
       (store (make_gnirt) comp_search)
       (store (make_gnirt) ans))

(defun get_whole_string (attempt completion_table start prefix answer)
       (cat_into comp_search prefix attempt)
       (set_loc completion_table start)
       (set_loc completion_table
                (iferror (search completion_table attempt) (return_value -1)))
       (iferror (search completion_table attempt)
                (add_to_loc completion_table (sub 0 (length attempt)))
                (delete answer 10000)
                (insert_region completion_table
                               (location completion_table)
                               (find_first_in_fa completion_table "   
")
                               answer)
                (return_value 1))
       (return_value 0))

(defun try_comp ( &aux code )
loop   (cond ((eq (store (get_whole_string (read_line "string: " 27)
                                           current_buffer 0 13 ans)
                         code) 0)
              (print_msg "ambiguous" 0))
             ((eq code -1) (print_msg "no such string" 0))
             ((t) (print_msg ans 0)))
       (goto loop))
^L