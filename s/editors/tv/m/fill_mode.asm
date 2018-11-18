
(documentation ">doc>sm>fill_mode.doc")

(variable back_word char current_buffer)
(variable keep_on_tunneling token_hackers white_space)
(variable fill_width indent_column)

(static position)

(defun user_init ()
       (store 65 fill_width)
       (store 0 indent_column)
       (store (make_mark) position))

(defun fill_mode ( &aux token_hackers )
       (store (cons 'check_filling token_hackers) token_hackers)
       (keep_on_tunneling))

(defun check_filling ()
       (cond ((and (or (eq char " ") (eq char 9))
                   (gp (get_hpos current_buffer (location current_buffer))
                         fill_width))
               (set_mark position current_buffer (location current_buffer))
               (set_loc current_buffer (iferror
                    (find_first_in_ba current_buffer white_space) 0))
               (cond ((eq (nthr current_buffer -1) " ")
                         (delete current_buffer -1)))
               (insert 13 current_buffer)
               (move_over indent_column)
               (set_loc current_buffer (eval_mark position current_buffer)))))


(defun move_over (amount)
     (do_while (gp amount 0)
          (insert 32 current_buffer)
          (store (sub amount 1) amount)))
^L