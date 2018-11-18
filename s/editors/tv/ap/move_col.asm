
(variable argument argumentp beginning_of_line current_buffer)
(variable end_of_line next_line)
(static check_col)

(documentation ">doc>sm>move_col.doc")

(defun user_init ()
     (store 66 check_col))

(defun move_col (&aux were_here)
       (store (location current_buffer) were_here)
       (cond (argumentp (store (add 1 argument) check_col)))
       (store 0 argument)
       (cond ((gep (get_hpos current_buffer (location current_buffer))
                   check_col)
              (next_line)))
       (do_while (lp (location current_buffer) (length current_buffer))
                 (end_of_line)
                 (cond ((gp (get_hpos current_buffer (location current_buffer))
                            check_col)
                        (beginning_of_line)
                        (do_while (lp (get_hpos current_buffer
                                      (location current_buffer)) check_col)
                                  (add_to_loc current_buffer 1))
                        (return)))
                 (next_line))
       (set_loc current_buffer were_here)
       (return))


^L