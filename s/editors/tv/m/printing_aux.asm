
(variable dispatch default_d M_dispatch default_md)
(variable argument back_char back_word beginning_of_line call_list char)
(variable current_buffer delete_char display_mode_line do_cr do_tab)
(variable end_of_line forward_char forward_word full_redisplay)
(variable keep_on_tunneling read_line redo_mode_line rubout_char)
(variable self_insert token_chars token_hackers verbose_printing)

(variable cursor_char display_read_line)

(documentation ">doc>sm>tv.doc")

(defun printing_aux_start ()
     (store "@" cursor_char)
     (store read_line display_read_line)
     (store tty_read_line read_line)
     (as 'print_mode_line dispatch 29)       ;C-]
     (as 'print_mode_line default_d 29)
     (as 'echo_line_cr M_dispatch 13)        ;M-C-m
     (as 'echo_line_cr default_md 13)
     (as 'tty_back_word M_dispatch 66)       ;M-B
     (as 'tty_back_word default_md 66)
     (as 'tty_back_word M_dispatch 98)       ;M-b
     (as 'tty_back_word default_md 98)
     (as 'tty_forward_word M_dispatch 70)    ;M-F
     (as 'tty_forward_word default_md 70)
     (as 'tty_forward_word M_dispatch 102)   ;M-f
     (as 'tty_forward_word default_md 102))


(defun do_cr ()
     (call_list token_hackers)
     (insert 13 current_buffer)
     (cond (verbose_printing (tyo 13))))

(defun do_tab ()
     (call_list token_hackers)
     (insert 9 current_buffer)
     (cond (verbose_printing (tyo 9))))

(defun self_insert ()
     (insert char current_buffer)
     (cond (verbose_printing (tyo char))))

(defun forward_char ()
     (cond (verbose_printing (tyo (nthr current_buffer 0))))
     (add_to_loc current_buffer 1))

(defun back_char ()
     (cond (verbose_printing (tyo (nthr current_buffer -1))))
     (add_to_loc current_buffer -1))

(defun tty_forward_word (&aux loc)
     (cond ((not verbose_printing)
               (forward_word)
               (return)))
     (store (iferror (find_first_in_fa current_buffer token_chars)
          (length current_buffer)) loc)
     (do_while (lp (location current_buffer) loc)
          (tyo (nthr current_buffer 0))
          (add_to_loc current_buffer 1))
     (store (iferror (find_first_not_in_fa current_buffer token_chars)
          (length current_buffer)) loc)
     (do_while (lp (location current_buffer) loc)
          (tyo (nthr current_buffer 0))
          (add_to_loc current_buffer 1)))

(defun tty_back_word (&aux loc)
     (cond ((not verbose_printing)
               (back_word)
               (return)))
     (store (iferror (find_first_in_ba current_buffer token_chars) 0) loc)
     (do_while (gp (location current_buffer) loc)
          (tyo (nthr current_buffer -1))
          (add_to_loc current_buffer -1))
     (store (iferror (find_first_not_in_ba current_buffer token_chars) 0) loc)
     (do_while (gp (location current_buffer) loc)
          (tyo (nthr current_buffer -1))
          (add_to_loc current_buffer -1)))

(defun delete_char ()
     (cond (verbose_printing
               (tyo "/")
               (tyo (nthr current_buffer 0))))
     (delete current_buffer 1))

(defun rubout_char ()
     (cond (verbose_printing
               (tyo "\")
               (tyo (nthr current_buffer -1))))
     (delete current_buffer -1))

(defun tty_read_line (prompt terminator &aux string)
     (tyo 13)
     (print prompt 0 0 0)
     (store (display_read_line prompt terminator) string)
     (tyo 13)
     (push string))

(defun full_redisplay (&aux end i loc)
     (store (location current_buffer) loc)
     (for i 1 argument
          (beginning_of_line)
          (add_to_loc current_buffer -1))
     (cond ((eq (location current_buffer) 0) (tyo 13)))
     (do_while (lp (location current_buffer) loc)
          (tyo (nthr current_buffer 0))
          (add_to_loc current_buffer 1))
     (tyo cursor_char)
     (for i 1 argument
          (end_of_line)
          (add_to_loc current_buffer 1))
     (store (location current_buffer) end)
     (set_loc current_buffer loc)
     (do_while (lp (location current_buffer) end)
          (tyo (nthr current_buffer 0))
          (add_to_loc current_buffer 1))
     (cond ((eq (location current_buffer) (length current_buffer)) (tyo 13)))
     (set_loc current_buffer loc)
     (store 0 argument))

(defun print_mode_line ()
     (tyo 13)
     (store 2 display_mode_line)
     (redo_mode_line)
     (tyo 13))

(defun echo_line_cr (&aux end)
     (store (location current_buffer) end)
     (beginning_of_line)
     (tyo 13)
     (do_while (lp (location current_buffer) end)
          (tyo (nthr current_buffer 0))
          (add_to_loc current_buffer 1))
     (tyo 13)
     (insert 13 current_buffer))

(defun set_cursor_char ()
     (tyo 13)
     (print "Set to? " 0 0 0)
     (store (tyo (tyi)) cursor_char)
     (tyo 13))
^L