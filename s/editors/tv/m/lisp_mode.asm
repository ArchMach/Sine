
(documentation ">doc>sm>lisp_mode.doc")

(variable white_space current_buffer add_mode_fun)
(variable beginning_of_line forward_s_expr back_s_expr)
(variable asm_mode keep_on_tunneling)
(variable dispatch M_dispatch C-X_dispatch)

(defun indent_for_lisp ( &aux here begin save hpos first_time )
       (beginning_of_line)
       (delete current_buffer
               (min (iferror (find_first_not_in_fr current_buffer white_space)
                             (sub (length current_buffer)
                                  (location current_buffer)))
                    (sub (searchr current_buffer 13) 1)))
       (store (location current_buffer) here)
       (back_over_lisp_wsac)
loop   (cond ((eq (nthr current_buffer -1) \28)
              (cond ((not (eq (nthr current_buffer 0) \28))
                     (store (location current_buffer) save)
                     (store (iferror (search current_buffer 13)
                                     (length current_buffer))
                            begin)
                     (forward_s_expr)
                     (over_white_space)
                     (cond ((or (lep begin (location current_buffer))
                                (eq (nthr current_buffer 0) ";"))
                            (set_loc current_buffer save)))))
              (goto go_here)))
       (store (location current_buffer) save)
       (store (iferror (rsearch current_buffer 13) 0) begin)
       (back_white_space)
       (cond ((lp begin (location current_buffer))
              (back_s_expr)             ;still on same line
              (goto loop)))
       (set_loc current_buffer save)
go_here
       (store (get_hpos current_buffer (location current_buffer)) hpos)
       (set_loc current_buffer here)
       (insert_white_space_to_hpos hpos))

(defun lisp_newline ()
       (insert 13 current_buffer)
       (indent_for_lisp))

(defun user_init ()
       (store lisp_mode asm_mode)
       (store (fill_char_array (make_array 1 128) "    
") white_space))

(defun lisp_mode ()
       (bind_key m-c-i indent_for_lisp)
       (bind_key c-j lisp_newline)
       (keep_on_tunneling))

(defun insert_white_space_to_hpos (hpos &aux i)
       (store (sub hpos 1) hpos)
       (for i 1 (div hpos 5) (insert 9 current_buffer))
       (for i 1 (mod hpos 5) (insert 32 current_buffer)))

(defun over_white_space ()
     (set_loc current_buffer
              (find_first_not_in_fa current_buffer white_space)))

(defun back_white_space ()
     (set_loc current_buffer
              (find_first_not_in_ba current_buffer white_space)))

(defun back_over_lisp_wsac ( &aux semi )
loop (back_white_space)
     (cond ((gp (store (iferror (rsearch current_buffer ";") (push 0)) semi)
                (rsearch current_buffer 13))
            (set_loc current_buffer semi)
            (goto loop)))                    ;keep looking
     (back_white_space))
^L