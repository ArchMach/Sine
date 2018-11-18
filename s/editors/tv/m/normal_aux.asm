
(variable dispatch M_dispatch O_dispatch C-X_dispatch)
(variable alphanumerics argument argumentp back_word)
(variable beginning_of_line current_buffer current_mark)
(variable emacs_type_t end_of_line forward_word hold keep_on_tunneling)
(variable library_dir load_file previous_line replace)
(variable replace_all_util self_insert tg token_chars white_space)

(variable fill_width indent_column)
(static text_mode_token_table tg1 tm1)

(documentation ">doc>sm>tv.doc")

(defun user_init ()
       (load_file (cat_into tg library_dir ">replace.sine"))
       (store 65 fill_width)
       (store 0 indent_column)
       (store (make_mark) tm1)
       (store (make_gnirt) tg1)
       (store (fill_char_array (make_array 1 128)
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890'-%$#@_")
              text_mode_token_table)
       (fill_char_array text_mode_token_table \22))    ;double quote

(defun normal_aux_start ( &aux token_chars )
       (store text_mode_token_table token_chars)
       (bind_key C-t twiddle_chars)
       (bind_array_cell C-X_dispatch "." 'set_indent_column)
       (bind_array_cell C-X_dispatch "=" 'print_current_position)
       (bind_array_cell C-X_dispatch "f" 'set_fill_width)
       (bind_array_cell C-X_dispatch "F" 'set_fill_width)
       (bind_array_cell M_dispatch 34 'ditto)     ;34="
       (bind_key M-a back_sentence)
       (bind_key M-A back_sentence)
       (bind_key M-c capitalize_word)
       (bind_key M-C capitalize_word)
       (bind_key M-e forward_sentence)
       (bind_key M-E forward_sentence)
;      (bind_key M-g fill_region)
;      (bind_key M-G fill_region)
       (bind_key M-h set_point_paragraph)
       (bind_key M-H set_point_paragraph)
       (bind_key M-l lower_case_word)
       (bind_key M-L lower_case_word)
       (bind_key M-q fill_paragraph)
       (bind_key M-Q fill_paragraph)
       (bind_key M-s center_line)
       (bind_key M-S center_line)
       (bind_key M-t twiddle_word)
       (bind_key M-T twiddle_word)
       (bind_key M-u upper_case_word)
       (bind_key M-U upper_case_word)
       (bind_key M-[ back_paragraph)
       (bind_key M-\ delete_whitespace)
       (bind_key M-] forward_paragraph)
       (keep_on_tunneling))


(defun twiddle_chars (&aux char)
     (cond ((not emacs_type_t) (add_to_loc current_buffer -1)))
     (cond
          ((eq (location current_buffer) 0) (set_loc current_buffer 1))
          ((eq (location current_buffer) (length current_buffer))
               (add_to_loc current_buffer -1))
          ((eq (nthr current_buffer -1) 13) (add_to_loc current_buffer 1))
          ((eq (nthr current_buffer 0) 13) (add_to_loc current_buffer -1)))
     (store (nthr current_buffer -1) char)
     (delete current_buffer -1)
     (add_to_loc current_buffer 1)
     (insert char current_buffer))


(defun set_indent_column ()
     (store argument indent_column))


(defun print_current_position (&aux loc len)
     (store 1 hold)                ;point, length, percent, hpos, mark
     (store (location current_buffer) loc)
     (store (length current_buffer) len)
     (delete tg1 (length tg1))
     (insert_ioa "location is ^i" loc tg1)
     (insert_ioa ",  length is ^i" len tg1)
     (insert_ioa " (^i%)" (div (mul loc 100) len) tg1)
     (insert_ioa ",  hpos is ^i" (get_hpos current_buffer loc) tg1)
     (insert_ioa ",  mark at ^i  " (eval_mark current_mark current_buffer) tg1)
     (print tg1 1 1 0))


(defun set_fill_width ()
     (cond (argumentp (store argument fill_width))
           ((t) (store (get_hpos current_buffer
                    (location current_buffer)) fill_width)))
     (store 0 argument))


(defun ditto (&aux loc hpos begin end i)
     (store (location current_buffer) loc)
     (store (get_hpos current_buffer loc) hpos)
     (previous_line)
     (do_while (lp (get_hpos current_buffer (location current_buffer)) hpos)
          (add_to_loc current_buffer 1))
     (add_to_loc current_buffer 1)
     (back_word)
     (store (location current_buffer) begin)
     (for i 1 argument (forward_word))
     (store (location current_buffer) end)
     (set_loc current_buffer loc)
     (insert_region current_buffer begin end current_buffer)
     (store 0 argument))


(defun back_sentence ()
     (set_loc current_buffer
          (iferror (find_first_in_ba current_buffer alphanumerics) 0))
     again
     (set_loc current_buffer
          (iferror (find_first_in_ba current_buffer ".!?") 0))
     (cond
          ((end_sentencep)
               (set_loc current_buffer
                    (find_first_not_in_fa current_buffer white_space))
               (return))
          ((eq (location current_buffer) 0)
               (return))
          ((t)
               (set_loc current_buffer (sub (location current_buffer) 1))
               (goto again))))


;check for end of sentence

(defun end_sentencep (&aux offset)      ;this has side effects!
     (store 0 offset)
     again
     (cond
          ((eq (nthr current_buffer offset) 13)   ;13=<cr>
               (add_to_loc current_buffer offset)
               (return_value (t)))
          ((eq (nthr current_buffer offset) 32)   ;32=<sp>
               (add_to_loc current_buffer offset)
               (return_value (t)))
          ((or (eq (nthr current_buffer offset) 34)    ;34="
               (eq (nthr current_buffer offset) 39))   ;39='
               (store (add 1 offset) offset)
               (goto again))
          ((or (eq (nthr current_buffer offset) 41)    ;41=)
               (eq (nthr current_buffer offset) 96))   ;96=`
               (store (add 1 offset) offset)
               (goto again))
          ((t)
               (return_value (nil)))))


(defun capitalize_word ( &aux c)
       (set_loc current_buffer
                (find_first_in_fa current_buffer token_chars))
       (cond ((and (gep (store (nthr current_buffer 0) c) "a") (lep c "z"))
              (delete current_buffer 1)
              (insert (sub c 32) current_buffer))
             ((t) (add_to_loc current_buffer 1)))
       (cond ((eq 0 (iferror (find_first_not_in_fr current_buffer token_chars)
                              1))
               (return)))                    ;to do 1 char words properly
       (lower_case_word))


(defun forward_sentence ()
     again
     (set_loc current_buffer
          (iferror (add 1 (find_first_in_fa current_buffer ".!?"))
                   (length current_buffer)))
     (cond
          ((end_sentencep)
               (return))
          ((eq (location current_buffer) (length current_buffer))
               (return))
          ((t)
               (set_loc current_buffer (add (location current_buffer) 1))
               (goto again))))


(defun set_point_paragraph ()
     (forward_paragraph)
     (set_mark current_mark current_buffer (location current_buffer))
     (back_paragraph)
     (beginning_of_line))


(defun lower_case_word ( &aux c )
       (set_loc current_buffer
                (find_first_in_fa current_buffer token_chars))
       (do_while (eq (ar token_chars (store (nthr current_buffer 0) c)) 1)
                 (cond ((and (gep c "A") (lep c "Z"))
                        (delete current_buffer 1)
                        (insert (add c 32) current_buffer))
                       ((add_to_loc current_buffer 1)))))


(defun fill_paragraph(&aux end_mark)
       (cond (argumentp (store argument fill_width)))
       (store 0 argument)
       (set_mark tm1 current_buffer (location current_buffer))

       (forward_paragraph)
       (cond ((eq (nthr current_buffer -1) 13)
          (add_to_loc current_buffer -1)))   ;<cr> at end of
                              ;buffer is a paragraph delimiter.
       (store (make_mark) end_mark)
       (set_mark end_mark current_buffer (location current_buffer))
       (back_paragraph)

loop
       (forward_word)
       (cond ((gp (get_hpos current_buffer (location current_buffer))
                  fill_width)
          (back_word)
          (cond ((eq (nthr current_buffer -1) " ") (delete_whitespace)))
          (insert 13 current_buffer)
          (move_over indent_column)
          (forward_word)))
       (cond ((lp (iferror (find_first_in_fa current_buffer 13)
                           (add 1 (length current_buffer)))
                                   ;don't do it if there is no <cr>
                  (min (iferror (find_first_in_fa current_buffer token_chars)
                                (length current_buffer))
                       (eval_mark end_mark current_buffer)))
          (set_loc current_buffer (find_first_in_fa current_buffer 13))
          (delete current_buffer 1)
          (delete_whitespace)
          (insert " " current_buffer)))
       (cond ((lp (location current_buffer)
                  (eval_mark end_mark current_buffer))
          (goto loop)))

       (set_loc current_buffer (eval_mark tm1 current_buffer)))


(defun center_line (&aux loc loc_eol len_text)
     (cond
          (argumentp (store argument fill_width))
          ((t) (store fill_width argument)))
     (store (sub argument indent_column) argument)
     (cond ((lp argument 1) (return)))
     (beginning_of_line)
     (store (location current_buffer) loc)
     (delete current_buffer (find_first_not_in_fr current_buffer "     "))
                                        ;that's a tab and a blank
     (store (sub (iferror (search current_buffer 13)
                          (add 1 (length current_buffer))) 1) loc_eol)
     (cond
          ((eq loc loc_eol) (return))
          ((gp 0 (store (sub argument (sub loc_eol loc)) len_text)) (return)))
     (move_over (add (div len_text 2) indent_column))
     (end_of_line)
     (store 0 argument))


;add whitespace to the left of a line

(defun move_over (amount)
     (do_while (gp amount 0)
          (insert 32 current_buffer)
          (store (sub amount 1) amount)))


(defun twiddle_word (&aux word saved)
     (cond ((not emacs_type_t) (back_word)))
     (store (make_gnirt) word)
     (set_loc current_buffer (find_first_in_ba current_buffer token_chars))
     (insert_region current_buffer
          (find_first_not_in_ba current_buffer token_chars)
          (location current_buffer) word)
     (delete current_buffer (sub 0 (length word)))
     (store (location current_buffer) saved)
     (set_loc current_buffer (find_first_in_fa current_buffer token_chars))
     (insert word current_buffer)
     (delete word (length word))
     (insert_region current_buffer
          (location current_buffer)
          (iferror (find_first_not_in_fa current_buffer token_chars)
                   (length current_buffer))
          word)
     (delete current_buffer (length word))
     (set_loc current_buffer saved)
     (insert word current_buffer)
     (forward_word))

(defun upper_case_word ( &aux c )
       (set_loc current_buffer
                (find_first_in_fa current_buffer token_chars))
       (do_while (eq (ar token_chars (store (nthr current_buffer 0) c)) 1)
                 (cond ((and (gep c "a") (lep c "z"))
                        (delete current_buffer 1)
                        (insert (sub c 32) current_buffer))
                       ((add_to_loc current_buffer 1)))))


(defun back_paragraph ()
     (set_loc current_buffer
          (iferror (find_first_not_in_ba current_buffer white_space) 0))
     again
     (set_loc current_buffer
          (iferror (add 1 (rsearch current_buffer 13)) 0))
     (cond
          ((end_paragraphp)
               (set_loc current_buffer
                    (find_first_not_in_fa current_buffer white_space))
               (return))
          ((eq (location current_buffer) 0)
               (return))
          ((t)
               (add_to_loc current_buffer -1)
               (goto again))))


;check for end of paragraph

(defun end_paragraphp ()
     (cond
          ((eq (nthr current_buffer 0) 9)    ;9=<tab>
               (return_value (t)))
          ((eq (nthr current_buffer 0) 13)   ;13=<cr>
               (return_value (t)))
          ((eq (nthr current_buffer 0) ".")  ;nite commands
               (return_value (t)))
          ((t) (return_value (nil)))))


(defun delete_whitespace ()
     (delete current_buffer (iferror
          (find_first_not_in_br current_buffer "  ")   ;<sp> and <tab>
          (location current_buffer)))
     (delete current_buffer (iferror
          (find_first_not_in_fr current_buffer "  ")
          (sub (length current_buffer) (location current_buffer)))))


(defun forward_paragraph ()
     (set_loc current_buffer
          (iferror (find_first_not_in_fa current_buffer white_space)
               (length current_buffer)))
     again
     (set_loc current_buffer
          (iferror (search current_buffer 13) (length current_buffer)))
     (cond
          ((end_paragraphp)
               (set_loc current_buffer
                    (find_first_not_in_ba current_buffer white_space))
               (return))
          ((eq (location current_buffer) (length current_buffer))
               (return))
          ((t)
               (add_to_loc current_buffer 1)
               (goto again))))

^L