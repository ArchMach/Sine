
(variable dispatch C-X_dispatch M_dispatch O_dispatch)
(variable default_d default_cxd default_md default_od)
(static tunnel_path)
(static loaded_modes)
(variable editor_name)
(variable buffer_list)
(variable current_tvbuf)      ;structure containing info about current buffer
(variable current_buffer current_screen current_mark current_filename
          current_modifiedp current_mode)
(variable last_buffer_in)
(static message)
(static clear_modified)
(variable echo_buf echo_screen echo_window echo_mark)
(static old_read_line)
(variable linel pagel)
(variable kill_ring kill_buffer)
(variable search_string)
(variable char)
(variable argument argumentp)
(static save_delete_flag old_save_delete_flag)
(variable cmnd_1 cmnd_2 old_cmnd_1 old_cmnd_2)
     ;current command (cmnd_2 is used for M, O, and C-x dispatch) and
     ;previous command
(variable tg)
(static read_line_tg)
(static abort_flag)
(variable hold)
(variable function_to_call)
(variable display_mode_line)
(variable recursive_read_line)
(variable first_time)
(variable page_overlap)
(variable library_dir)
(variable default_mode)
(static normal_aux_loaded normal_aux_loading)
(variable printingp verbose_printing printing_aux_start)
(variable white_space alphanumerics token_chars)
(variable token_hackers)
(variable desired_column emacs_type_np emacs_type_t)
(variable try_all_modes modes_to_try)

(documentation ">doc>sm>tv.doc")

;;;  command_args are defined as follows
;;;       1-5) command line arguments
;;;       6) flags bits.  bit 31 is tb_mode, bit 30 is force read,
;;;            bit 29 is display terminal
;;;       7) pathname of the startup.
;;;       8) pagel
;;;       9) linel

;;;  default startup which hack command args like tv

(defun startup (&aux filename i)
       (cond (first_time
              (user_init)
              (cond ((eq (command_args 1) 0)
                     (goto_buffer "main")
                     (return)))))
       (for i 1 5
            (cond ((eq (store (command_args i) filename) 0)
                   (return))
                  ((eq (and (command_args 6) 2) 2)          ;force read bit
                   (goto_buffer (derive_buffer_name filename))
                   (goto read_it))
                  ((eq (and (command_args 6) 1) 0)          ;tb_mode bit
                   (goto_buffer "main")
read_it            (replace current_filename filename)
                   (punt_changes))
                  ((t) (find_file filename)))))

(defun user_init ())

(defun top_level (&aux filename restarting i )
       (store (cons "Normal" 0) loaded_modes)
       (store 0 modes_to_try)
       (store (t) try_all_modes)
       (store "main" last_buffer_in)
       (store 0 buffer_list)
       (store 0 current_tvbuf)
       (store (command_args 8) pagel)
       (store (make_buffer) echo_buf)
       (store (make_screen echo_buf) echo_screen)
       (display_screen echo_screen pagel 1)
       (store (make_window echo_buf) echo_window)
       (store (make_mark) echo_mark)
       (store (make_gnirt) old_read_line)
       (store (make_gnirt) search_string)
       (store (make_gnirt) tg)
       (store (make_gnirt) read_line_tg)
       (store (make_gnirt) message)
       (store (make_buffer) kill_buffer)
       (store (make_circle 10) kill_ring)
       (store 0 old_save_delete_flag)
       (store 0 save_delete_flag)
       (store 0 hold)
       (store -1 display_mode_line)
       (store 0 recursive_read_line)
       (store -1 old_cmnd_1)
       (store -1 old_cmnd_2)
       (store (nil) emacs_type_np)
       (store (t) emacs_type_t)
       (store (nil) clear_modified)
       (store (t) first_time)
       (store (nil) normal_aux_loaded)
       (store (nil) normal_aux_loading)
       (store 11 page_overlap)
       (store "Normal" default_mode)
       (store ">sl1>sinemacs" library_dir)
       (store "TVmacs" editor_name)
       (store (fill_char_array (make_array 1 128) "    
") white_space)
       (store (fill_char_array (make_array 1 128)
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
              alphanumerics)
       (store (fill_char_array (copy_array alphanumerics 0) "_!?")
              token_chars)

       (fill_vbl_array (store (make_array 32 128) default_d) 128
 
;commands

set_buffer_mark beginning_of_line back_char get_ortho_cmd        ;^@
delete_char end_of_line forward_char abort                       ;^D
back_char do_tab cr_tab kill_line                                ;^H
full_redisplay do_cr next_line_command open_line                 ;^L
previous_line_command quote_next_character reverse_string_search string_search ;^P
auto_load_rest get_multiplier next_page kill_point_to_mark       ;^T
get_C-X_cmd yank_last_kill return_from_reader get_meta_cmd       ;^X
illegal_command illegal_command illegal_command get_ortho_cmd    ;^\

;printable characters

self_insert_break self_insert_break self_insert_break self_insert_break
self_insert_break self_insert_break self_insert_break self_insert_break
self_insert_break self_insert_break self_insert_break self_insert_break
self_insert_break self_insert_break self_insert_break self_insert_break
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert_break self_insert_break
self_insert_break self_insert_break self_insert_break self_insert_break
self_insert_break self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert_break
self_insert_break self_insert_break self_insert_break self_insert_break
self_insert_break self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert
self_insert self_insert self_insert self_insert_break
self_insert_break self_insert_break self_insert_break rubout_char     )

       (fill_vbl_array (store (make_array 32 128) default_cxd) 128

illegal_command illegal_command list_buffers return_from_reader
illegal_command execute_command_line find_file_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command get_file save_buffer
illegal_command illegal_command illegal_command put_file
switch_point_and_mark print_buffer illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command

illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command auto_load_rest illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command auto_load_rest illegal_command illegal_command

illegal_command illegal_command switch_buffers illegal_command
"dired" illegal_command auto_load_rest illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command set_major_mode illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
"tags" illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command

illegal_command illegal_command switch_buffers illegal_command
"dired" illegal_command auto_load_rest illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command add_minor_mode illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command  )

       (fill_vbl_array (store (make_array 32 128) default_md) 128

illegal_command illegal_command back_s_expr illegal_command
delete_s_expr illegal_command forward_s_expr illegal_command
illegal_command illegal_command illegal_command illegal_command
load_function illegal_command illegal_command illegal_command
illegal_command illegal_command "query_replace" illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command rubout_s_expr

illegal_command illegal_command auto_load_rest illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
top_of_buffer illegal_command bottom_of_buffer "print_doc"

illegal_command auto_load_rest back_word auto_load_rest
delete_word auto_load_rest forward_word illegal_command
auto_load_rest illegal_command illegal_command illegal_command
auto_load_rest illegal_command illegal_command illegal_command
illegal_command auto_load_rest "global_replace" auto_load_rest
auto_load_rest auto_load_rest previous_page illegal_command
call_function yank_previous_kill illegal_command auto_load_rest
auto_load_rest auto_load_rest illegal_command illegal_command

illegal_command auto_load_rest back_word auto_load_rest
delete_word auto_load_rest forward_word illegal_command
auto_load_rest illegal_command illegal_command illegal_command
auto_load_rest illegal_command illegal_command illegal_command
illegal_command auto_load_rest "global_replace" auto_load_rest
auto_load_rest auto_load_rest previous_page illegal_command
call_function yank_previous_kill illegal_command illegal_command
illegal_command illegal_command illegal_command rubout_word )

       (fill_vbl_array (store (make_array 32 128) default_od) 128
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command

illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command

illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command

illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command
illegal_command illegal_command illegal_command illegal_command)



       (store (copy_array default_d 0) dispatch)
       (store (copy_array default_md 0) M_dispatch)
       (store (copy_array default_od 0) O_dispatch)
       (store (copy_array default_cxd 0) C-X_dispatch)

       (store (t) verbose_printing)
       (store (nil) printingp)
       (cond ((eq (and (command_args 6) 4) 0)
               (store (t) printingp)
               (cat_into tg library_dir ">" "printing_aux.sine")
               (load tg)
               (call_vbl 'printing_aux_start 0)))

     (load (command_args 7))                 ;get the startup

reader_loop
       (store (command_args 9) linel)
       (store (command_args 8) pagel)
       (store 1 restarting)
       (errset "breal   " ((restart_at unwind_address)
loop                     (errset "abort"
                              ((cond ((eq restarting 1)
                                      (startup)
                                      (cond (first_time
                                             (store (nil) first_time)))))
                               (store (cadr current_tvbuf) current_buffer)
                               (store (car (cddr current_tvbuf))
                                      current_screen)
                               (display_screen current_screen 1 (sub pagel 2))
                               (store (car (cddr (cddr (cddr current_tvbuf))))
                                      current_mode)
                               (cond ((eq restarting 1) (store (nil) argumentp)
                                                        (full_redisplay)
                                                        (store 0 restarting)))
                               (store 0 recursive_read_line)
                               (invoke_editor 0))
                              ((goto loop))))
                        ())
     (print_clearing "" 1 pagel 0)
     (restart_at reader_loop)
     (return)

unwind_address           ;this clears out the stack.
     (signal "abort"))

(defun invoke_editor (alloc &aux O_dispatch
                            dispatch M_dispatch C-X_dispatch i token_hackers)
       (cond ((eq alloc 1)
              (store (copy_array default_d 0) dispatch)
              (store (copy_array default_md 0) M_dispatch)
              (store (copy_array default_od 0) O_dispatch)
              (store (copy_array default_cxd 0) C-X_dispatch)))
loop
       (store 0 token_hackers)
       (store (cdr current_mode) tunnel_path)
       (keep_on_tunneling)
       (store (cadr current_tvbuf) current_buffer)
       (store (car (cddr current_tvbuf)) current_screen)
       (store (car (cddr (cddr (cddr current_tvbuf)))) current_mode)
       (ift (eq abort_flag 3) loop))

(defun keep_on_tunneling ( &aux next_tunnel_link )
       (cond (normal_aux_loading (finish_auto_load_rest))
             ((eq tunnel_path 0) (reader current_buffer current_screen))
             ((t)
              (store (car tunnel_path) next_tunnel_link)
              (store (cdr tunnel_path) tunnel_path)
              (call_vbl next_tunnel_link 0))))

(defun Normal_mode ()
       (keep_on_tunneling))

(defun reader (current_buffer current_screen &aux i function_to_call argument
                                                  argumentp )
loop (cond ((and (lp hold 1) (eq (tyis) 0))
               (redo_mode_line)
               (display)))
     (store save_delete_flag old_save_delete_flag)
     (store 0 save_delete_flag)
     (store 0 abort_flag)
     (store 1 argument)       ;repeat once by default
     (store (nil) argumentp)
     (get_function dispatch (store (tyi) char))
     (store cmnd_1 old_cmnd_1)
     (store cmnd_2 old_cmnd_2)
     (store char cmnd_1)
     (store -1 cmnd_2)
     (check_each_cmd)
     (cond ((eq hold -1) (print_msg 32 0)))
     (store 0 hold)
     (errset "abort"
          ((for i 1 argument (call_vbl function_to_call 0)))
          ((store 0 abort_flag)))
     (store (or current_modifiedp (modifiedp current_buffer))
            current_modifiedp)
     (cond (clear_modified (store (nil) current_modifiedp)
                           (store (nil) clear_modified)))

;;;  abort_flag:
;;;       0 usual thing just read another character.
;;;       1 error occured blow out of this reader.
;;;       2 just return from the reader.
;;;       3 return to change modes

     (cond ((eq abort_flag 0) (goto loop))
           ((eq abort_flag 1) (signal "abort"))
           ((eq abort_flag 2) (store 0 abort_flag))))

(defun get_number ()
       (store (t) argumentp)
       (store (sub char 48) argument)
read_number_loop
       (ift (lp (store (tyi) char) 48) just_return)
       (ift (gp char 58) just_return)
       (store (add (mul argument 10) (sub char 48)) argument)
       (goto read_number_loop)
just_return)

(defun get_multiplier (&aux sign)
       (store (t) argumentp)
       (store 4 argument)
       (store 1 sign)
get_mult_loop
       (ift (eq (store (tyi) char) "-") negative_argument)
       (ift (lp char 48) times_four)
       (ift (gp char 58) times_four)
       (get_number)
call_it
       (store (mul sign argument) argument)
       (store char cmnd_1)
       (call_vbl (get_function dispatch char) 0)
       (return)
times_four
       (ifnil (eq char 21) call_it)     ;not ^U and no number so do it
       (store (mul 4 argument) argument)
       (goto get_mult_loop)
negative_argument
       (store -1 sign)
       (goto get_mult_loop))

(defun get_C-X_cmd ()
     (store (tyi) cmnd_2)
     (call_vbl (get_function C-X_dispatch cmnd_2)  0))

(defun get_meta_cmd ()
     (store (tyi) cmnd_2)
     (call_vbl (get_function M_dispatch cmnd_2) 0))

(defun get_ortho_cmd ()
     (store (tyi) cmnd_2)
     (call_vbl (get_function O_dispatch cmnd_2) 0))

(defun check_each_cmd ())


;;;  Command functions

(defun self_insert_break ()
       (call_list token_hackers)
       (self_insert))

(defun call_list (list)
       (do_until (eq list 0)
                 (call_vbl (car list) 0)
                 (store (cdr list) list)))

(defun self_insert ()
       (insert char current_buffer))

(defun do_tab ()
       (call_list token_hackers)
       (insert 9 current_buffer))

(defun do_cr ()
       (call_list token_hackers)
       (insert 13 current_buffer))

(defun cr_tab ()
       (do_cr)
       (do_tab))

(defun quote_next_character (&aux char i)
     (store (tyi) char)
     (for i 1 argument (insert char current_buffer))
     (store 0 argument))

(defun open_line ()
     (insert 13 current_buffer) (add_to_loc current_buffer -1))

(defun illegal_command ()
     (store 0 argument)                 ;no repeats
     (print_msg "Illegal Command" 1))

;;;  ^F, ^B, ^D, rubout and company

(defun forward_char ()
     (add_to_loc current_buffer argument)
     (store 0 argument))

(defun forward_word ()
     (set_loc current_buffer (iferror
          (find_first_in_fa current_buffer token_chars)
          (length current_buffer)))
     (set_loc current_buffer (iferror
          (find_first_not_in_fa current_buffer token_chars)
          (length current_buffer))))

(defun forward_s_expr ()
     (add_to_loc current_buffer (over_s_expr_fr current_buffer)))

(defun back_char ()
     (add_to_loc current_buffer (sub 0 argument))
     (store 0 argument))

(defun back_word ()
     (set_loc current_buffer (iferror
          (find_first_in_ba current_buffer token_chars) 0))
     (set_loc current_buffer (iferror
          (find_first_not_in_ba current_buffer token_chars) 0)))

(defun back_s_expr ()
     (add_to_loc current_buffer (over_s_expr_br current_buffer)))

(defun delete_char ()
     (delete current_buffer argument)
     (store 0 argument))

(defun delete_word ( &aux first point )
     (store (location current_buffer) point)
     (add_to_loc current_buffer
                 (store (find_first_in_fr current_buffer token_chars) first))
     (store (add first (find_first_not_in_fr current_buffer token_chars))
            first)
     (set_loc current_buffer point)
     (save_deletes current_buffer first))

(defun delete_s_expr ()
     (save_deletes current_buffer (over_s_expr_fr current_buffer)))

(defun rubout_char ()
     (delete current_buffer (sub 0 argument))
     (store 0 argument))

(defun rubout_word ( &aux first point )
     (store (location current_buffer) point)
     (add_to_loc current_buffer
                 (store (find_first_in_br current_buffer token_chars) first))
     (store (add first (find_first_not_in_br current_buffer token_chars))
            first)
     (set_loc current_buffer point)
     (save_deletes current_buffer first))

(defun rubout_s_expr ()
     (save_deletes current_buffer (over_s_expr_br current_buffer)))

(defun over_s_expr_fr (buf &aux open_paren start_token old_loc)
     (store (location buf) old_loc)
     (store (iferror (search buf 40) (length buf)) open_paren)
     (store (add 1 (iferror (find_first_in_fa buf token_chars)
                            (length buf))) start_token)
     (cond ((lp open_paren start_token)
            (set_loc buf open_paren)
            (match_parens_f buf)
            (sub (location buf) (set_loc buf old_loc)))
           ((t) (over_tok_fr buf))))

(defun over_tok_fr (buf &aux old_loc)
       (store (location buf) old_loc)
       (set_loc buf (find_first_in_fa buf token_chars))
       (sub (find_first_not_in_fa buf token_chars) (set_loc buf old_loc)))

(defun over_tok_br (buf &aux old_loc)
       (store (location buf) old_loc)
       (set_loc buf (find_first_in_ba buf token_chars))
       (sub (find_first_not_in_ba buf token_chars) (set_loc buf old_loc)))

(defun over_s_expr_br (buf &aux close_paren start_token old_loc)
     (store (location buf) old_loc)
     (store (iferror (rsearch buf 41) 0) close_paren)
     (store (add -1 (iferror (find_first_in_ba buf token_chars) 0))
            start_token)
     (cond ((gp close_paren start_token)
            (set_loc buf close_paren)
            (match_parens_b buf)
            (sub (location buf) (set_loc buf old_loc)))
           ((t) (over_tok_br buf))))

(defun match_parens_f (buf &aux open close)
loop
     (store (iferror (search buf 40) (length buf)) open)
     (store (iferror (search buf 41) (length buf)) close)
     (cond ((lp open close) (set_loc buf open) (match_parens_f buf))
           ((t) (set_loc buf close) (return)))
     (goto loop))

(defun match_parens_b (buf &aux open close)
loop
     (store (iferror (rsearch buf 40) 0) open)
     (store (iferror (rsearch buf 41) 0) close)
     (cond ((gp close open) (set_loc buf close) (match_parens_b buf))
           ((t) (set_loc buf open) (return)))
     (goto loop))

;;;  line commands

(defun beginning_of_line ()
     (set_loc current_buffer
          (iferror (add 1 (rsearch current_buffer 13)) 0)))

(defun end_of_line ()
     (set_loc current_buffer
          (iferror (sub (search current_buffer 13) 1) (length current_buffer))))

(defun next_line_command ( &aux i)
     (cond (emacs_type_np (get_desired_column)))
     (for i 1 argument
          (set_loc current_buffer
               (iferror (search current_buffer 13) (length current_buffer))))
     (store 0 argument)
     (cond (emacs_type_np (move_to_desired_column))))

(defun next_line ()
     (set_loc current_buffer
          (iferror (search current_buffer 13) (length current_buffer))))

(defun previous_line_command ( &aux i )
     (cond (emacs_type_np (get_desired_column)))
     (for i 1 argument
          (set_loc current_buffer
               (iferror (rsearch current_buffer 13) 0)))
     (beginning_of_line)
     (store 0 argument)
     (cond (emacs_type_np (move_to_desired_column))))

(defun previous_line ()
     (set_loc current_buffer
          (iferror (rsearch current_buffer 13) 0))
     (beginning_of_line))

(defun get_desired_column ()
     (ift (or (eq old_cmnd_1 14) (eq old_cmnd_1 16)) dont_reset)
     (ift (or (eq old_cmnd_1 22) (and (eq old_cmnd_1 27)    ;C-v, M-V and M-v
               (or (eq old_cmnd_2 86) (eq old_cmnd_2 118)))) dont_reset)
     (store (get_hpos current_buffer (location current_buffer)) desired_column)
dont_reset
     (return))

(defun move_to_desired_column ()
     (do_while (and
               (lp (get_hpos current_buffer (location current_buffer))
                    desired_column)
               (not (eq (nthr current_buffer 0) 13)))
          (iferror (add_to_loc current_buffer 1) (return))))

(defun next_page ( &aux i )
     (force_display current_screen -1)
     (store (mul (sub (sub pagel 2) page_overlap) argument) argument)
     (next_line_command)
     (store 0 argument))

(defun previous_page ( &aux i )
     (force_display current_screen -1)
     (store (mul (sub (sub pagel 2) page_overlap) argument) argument)
     (previous_line_command)
     (store 0 argument))

;;;  Mark mungers

(defun set_buffer_mark ()
     (set_mark current_mark current_buffer (location current_buffer)))

(defun switch_point_and_mark (&aux temp)
     (store (location current_buffer) temp)
     (set_loc current_buffer (eval_mark current_mark current_buffer))
     (set_mark current_mark current_buffer temp))

;;;  Moby deletes

(defun kill_line ()
     (save_deletes current_buffer
          (iferror (max 1 (sub (searchr current_buffer 13) 1))
                   (sub (length current_buffer) (location current_buffer)))))

(defun kill_point_to_mark ()
     (save_deletes current_buffer (sub (eval_mark current_mark current_buffer)
                                       (location current_buffer))))

;;;  Yank and stuff

(defun yank_last_kill ()
     (set_buffer_mark)
     (insert_region kill_buffer (low_kill_bound) (high_kill_bound)
                    current_buffer))

(defun yank_previous_kill ()
       (delete current_buffer      ;kill point to mark but don't save
                     (sub (eval_mark current_mark current_buffer)
                          (location current_buffer)))
       (store (cadr kill_ring) kill_ring)    ;pop
       (yank_last_kill))

(defun save_deletes (buf num &aux item)
     (ift (eq num 0) do_nothing)
     (cond ((and (eq old_save_delete_flag 0) (eq save_delete_flag 0))
;;;  start a new kill area
            (store (cddr kill_ring) kill_ring)    ;push to new kill area
            (set_loc kill_buffer (high_kill_bound))
            (delete kill_buffer (sub (low_kill_bound) (high_kill_bound)))))
     (cond ((lp num 0)        ;backward kill so insert at beginning of area
            (set_loc kill_buffer (low_kill_bound))
            (insert_region buf (add (location buf) num) (location buf)
                           kill_buffer)
            (ift (lp (high_kill_bound) (location kill_buffer)) bump_end))
          ((t)
           (set_loc kill_buffer (high_kill_bound))
           (insert_region buf (location buf) (add (location buf) num)
                          kill_buffer)
bump_end   (set_mark (car kill_ring)
                     kill_buffer (location kill_buffer))))
     (store 1 save_delete_flag)
     (delete buf num)
do_nothing)

(defun high_kill_bound ()
     (eval_mark (car kill_ring) kill_buffer))

(defun low_kill_bound ( &aux temp )
     (store (eval_mark (car (cdr (cdr kill_ring))) kill_buffer) temp)
     (cond ((gp temp (high_kill_bound)) (push 0))
           ((t) (push temp))))

;;;  buffer stuff

(defun switch_buffers (&aux nbn temp_last_buffer)
     (store (read_line "Buffer Name: " 13) nbn)
     (cond ((eq (length nbn) 0)
            (progn (insert last_buffer_in echo_buf)
                   (goto_buffer last_buffer_in)))
           ((t) (goto_buffer nbn))))

(defun list_buffers (&aux temp line cur)
     (rplaca (cdr (cddr (cddr current_tvbuf))) current_modifiedp)
     (store 1 hold)
     (store buffer_list temp)
     (store 1 line)
loop (print_clearing (car (store (car temp) cur)) 1 line 1)           ;name
     (cond ((cadr (cddr (cddr cur))) (print "*" 17 line 0)))          ;modp
     (delete tg 10000)
     (print (insert_ioa "^5i" (location (cadr cur)) tg) 19 line 0)    ;loc
     (delete tg 10000)
     (print (insert_ioa "^5i" (length (cadr cur)) tg) 25 line 0)      ;length
     (print (car (caar (cddr (cddr (cddr cur))))) 31 line 0)          ;mode
     (print (car (cddr (cddr cur))) 41 line 0)                        ;filename
     (store (add 1 line) line)
     (ifnil (eq (store (cdr temp) temp) 0) loop))

;;;  file hacking stuff

(defun get_file ( &aux nfn )
     (store (read_line "Input File Name: " 13) nfn)
     (cond ((gp (length nfn) 0) (replace current_filename nfn))
           ((t) (insert current_filename echo_buf)))
     (punt_changes))

(defun find_file_command ( &aux nfn )
     (store (read_line "Find File Name: " 13) nfn)
     (cond ((eq (length nfn) 0) 
            (replace tg current_filename)
            (insert current_filename echo_buf))
           ((t) (replace tg nfn)))
     (find_file tg))

(defun find_file (filename &aux nbn)
     (goto_buffer (derive_buffer_name filename))
     (cond ((eq (length current_buffer) 0))
           ((eq current_filename filename) (goto just_return))
           ((t) (errset "abort   "
                 ((store 
                    (read_line "Buffer Exists! Buffer Name To Use: " 13)
                    nbn))
                 ((goto just_return)))
                (cond ((gp (length nbn) 0) (goto_buffer nbn))
                      ((t) (insert (car current_tvbuf) echo_buf)))))
     (replace current_filename filename)
     (punt_changes)
     (store -1 display_mode_line)
just_return)

(defun punt_changes ( &aux mode_name )
     (store 1 clear_modified)
     (store -1 display_mode_line)
     (cond ((eq (read_file current_buffer current_filename) 0)
            (delete tg 10000)
            (print_msg (insert_ioa "^i Chars Read"
                                   (length current_buffer) tg) 0))
           ((t) (print_msg "File Not Found" 1)))
     
;;;  See if he has a particular mode in mind

     (set_loc current_buffer (search current_buffer 13))
     (set_loc current_buffer (iferror (rsearch current_buffer "-*-")
                                      (goto no_mode_spec)))
     (delete tg 10000)
     (add_mode (make_string        ;define a major mode
                (insert_region current_buffer
                               (add (rsearch current_buffer "-*-") 3)
                               (location current_buffer)
                               tg))
               1 1)
     (goto done)

no_mode_spec

;;;  since no specified mode try using the filename suffix (if user wants)

       (cond ((lep (length (store (get_filename_suffix current_filename)
                                  mode_name))
                   0)
              (goto add_default_mode))
             (try_all_modes
              (add_mode mode_name 1 -1)) ;;; don't bitch if not found
             ((member mode_name modes_to_try)
              (add_mode mode_name 1 1))
             ((t)
add_default_mode
              (add_mode default_mode 1 1)))

done   (set_loc current_buffer 0))

(defun put_file ( &aux nfn )
     (store (read_line "Output File Name: " 13) nfn)
     (cond ((gp (length nfn) 0) (replace current_filename nfn)
                                (store -1 display_mode_line))
           ((t) (insert current_filename echo_buf)))
     (save_changes))

(defun save_buffer ()
     (save_changes))          ;always save buffer
;    (cond (current_modifiedp (save_changes))
;          ((t) (print_msg "No Changes Made" 0))))

(defun save_changes ()
     (cond ((eq recursive_read_line 1) (blow_out "Won't Save Echo_buf")))
     (delete tg (length tg))
     (cond ((eq (write_file current_buffer current_filename) 0)
            (store (nil) current_modifiedp)
            (print_msg (insert_ioa "^i Chars Written"
                       (length current_buffer) tg) 0))
           ((t) (print_msg "Error In Output" 1))))

;;;  search

(defun string_search (&aux nss count times)
     (store argument times)
     (store 0 argument)
     (store (read_line "Search String: " 27) nss)
     (cond ((gp (length nss) 0) (replace search_string nss))
           ((t) (insert search_string echo_buf)))
     (for count 1 times
          (set_loc current_buffer
                   (iferror (search current_buffer search_string)
                             (progn (print_msg "Search Failed" 1) (return))))))

(defun reverse_string_search (&aux nss count times)
     (store argument times)
     (store 0 argument)
     (store (read_line "Reverse Search String: " 27) nss)
     (cond ((gp (length nss) 0) (replace search_string nss))
           ((t) (insert search_string echo_buf)))
     (for count 1 times
          (set_loc current_buffer
                   (iferror (rsearch current_buffer search_string)
                             (progn (print_msg "Search Failed" 1) (return))))))

;;;  top/bottom of buffer

(defun top_of_buffer ()
     (set_buffer_mark)
     (set_loc current_buffer 0))

(defun bottom_of_buffer ()
     (set_buffer_mark)
     (set_loc current_buffer (length current_buffer)))

(defun full_redisplay ()
     (store -1 display_mode_line)
     (force_display current_screen
                    (cond (argumentp (min argument (sub pagel 2)))
                          ((t) -1)))
     (force_display echo_screen -1))

(defun redo_mode_line ( &aux mode_line_num interval )
       (ift (eq display_mode_line 0) just_return)
       (store (sub pagel 1) mode_line_num)
       (ift (eq display_mode_line 1) just_msg)
       (print_clearing editor_name 1 mode_line_num 0)
       (tyo 32)
       (store \28 interval)
       (store (car current_mode) temp)
loop   (cond ((not (eq temp 0))
              (tyo interval)
              (print (car temp) 0 0 0)
              (store 32 interval)
              (store (cdr temp) temp)
              (goto loop)))
       (tyo \29)
       (tyo 32)
       (print_clearing (car current_tvbuf) 0 mode_line_num 0)
       (tyo \3a)
       (tyo 32)
       (print_clearing current_filename 0 mode_line_num 0)
just_msg
       (print_clearing message 60 mode_line_num 0)
       (store 0 display_mode_line)
just_return)

(defun abort ()
     (store 0 argument)
     (print_msg "Aborting" 1)
     (store 1 abort_flag))

(defun return_from_reader ()  ;tell the reader to return
     (store 2 abort_flag))

(defun print_buffer ()
     (store 1 hold)
     (print_clearing current_buffer 1 1 1))       ;do more processing

(defun execute_command_line ()
     (cline (read_line "External Command: " 13))
     (full_redisplay)
     (store 1 hold))

(defun load_function ()
       (replace tg (read_line "Sine File: " 13))
       (cond ((lp (load_file (insert ".sine" tg)) 0)
              (print_msg "File Not Found" 1)) ))

(defun call_function ( &aux fun)
     (cond ((functionp
             (eval (store (make_variable (read_line "Function Name: " 13))
                          fun)))
             (store fun function_to_call)
             (call_vbl fun 0))
           ((t)
             (cat_into tg library_dir ">" (get_pname fun) ".sine")
             (ift (lp (load_file tg) 0) error)
             (ifnil (functionp (eval fun)) error)
             (store fun function_to_call)
             (call_vbl fun 0)))
     (return)
error
     (print_msg "No Such Function" 1)
     (return))

;(defun print_binding (&aux string foo bar)
;    (cond ((eq (length (store (read_line "Key Description: " 13) string)) 0)
;           (return)))
;    (cond ((eq (car (store (decode_key string) foo)) 0)
;           (print_msg "Decoding Error" 1))
;          ((t) (print_msg
;                 (cond ((stringp (store (ar (car foo) (cdr foo)) bar))
;                        (cat_into tg 34 bar 34))
;                       ((t) (get_pname bar)))
;                 0))))

;;;  Utility functions

(defun goto_buffer (bufname &aux temp)
       (store -1 display_mode_line)
       (store buffer_list temp)
       (ift (eq current_tvbuf 0) loop)  ;if no current one
       (store (car current_tvbuf) last_buffer_in)
       (rplaca (cdr (cddr (cddr current_tvbuf))) current_modifiedp)
loop
       (cond ((eq temp 0)
              (store (cons (store (new_tvbuffer bufname "delete_me")
                                  current_tvbuf)
                           buffer_list)
                     buffer_list))
             ((eq (caar temp) bufname) (store (car temp) current_tvbuf))
             ((t) (store (cdr temp) temp) (goto loop)))
       (store (cadr current_tvbuf) current_buffer)
       (display_screen (store (car (cddr current_tvbuf)) current_screen)
                       1 (sub pagel 2))
       (store (cadr (cddr current_tvbuf)) current_mark)
       (store (car (cddr (cddr current_tvbuf))) current_filename)
       (store (cadr (cddr (cddr current_tvbuf))) current_modifiedp)
       (store (car (cddr (cddr (cddr current_tvbuf)))) current_mode)
       (store 3 abort_flag))

(defun new_tvbuffer (bufname filename &aux buf)

;;;  This is what a buffer looks like:
;;;       (name buffer screen mark filename mod (mode_names mode_defs))
;;;       Where mode_names is a list of strings which defines the major
;;;       and minor mode names, and mode_defs is a list of mode tunneling
;;;       functions.

     (cons (insert bufname (make_gnirt))
     (cons (store (make_buffer) buf)
     (cons (make_screen buf)
     (cons (set_mark (make_mark) buf 0)
     (cons (insert filename (make_gnirt))
     (cons (nil)
     (cons (cons (cons (insert "Normal" (make_gnirt)) 0)
                 (cons 'Normal_mode 0)) 0))))))))

(defun derive_buffer_name (filename &aux worst)
     (set_loc echo_buf 0)
     (delete echo_buf (length echo_buf))
     (insert filename echo_buf)
     (store (sub 0 (length filename)) worst)
     (add_to_loc echo_buf
                 (max (iferror (add 1 (rsearchr echo_buf 60)) (push worst))
                      (iferror (add 1 (rsearchr echo_buf 62)) (push worst))))
remove_dots
     (cond ((looking_atp 46 echo_buf 0)
            (add_to_loc echo_buf 1)
            (goto remove_dots)))
     (insert_region echo_buf
                    (location echo_buf)
                    (iferror (sub (search echo_buf 46) 1) (length filename))
                    (make_gnirt)))

(defun get_filename_suffix (filename &aux temp)
     (set_loc echo_buf 0)
     (delete echo_buf (length echo_buf))
     (insert filename echo_buf)
     (store (find_first_in_ba echo_buf "<>.") temp)    ;find last special char
     (insert_region echo_buf
                    (cond ((eq (nth echo_buf (sub temp 1)) ".")
                           (push temp))                ;we found a dot
                          ((t) (length echo_buf)))     ;no suffix
                    (length echo_buf)
                    (make_gnirt)))

(defun make_circle (nelm &aux first_elm last_elm i )
     (store (store (make_circle_elm 0) first_elm) last_elm)
     (for i 1 (sub nelm 1) (store (make_circle_elm last_elm) last_elm))
     (rplaca (cdr first_elm) last_elm)
     (rplacd (cdr last_elm) first_elm)
     (push first_elm))

(defun make_circle_elm (last &aux elm)
     (store (cons (set_mark (make_mark) kill_buffer 0)
                  (cons last 0)) elm)
     (ift (eq last 0) return_place)
     (rplacd (cdr last) elm)
return_place
     (push elm))

(defun read_line (prompt terminator
                         &aux current_modifiedp old_terminator dispatch
                              M_dispatch C-X_dispatch token_hackers tg tg1
                              O_dispatch)

; make current_modifiedp a temp so mod to echo_buffer don't get credited
; to real buffer.  Also get default bindings for everything

       (cond ((eq recursive_read_line 1) (blow_out "Recursive Read_line")))
       (store 1 recursive_read_line)
       (store default_d dispatch)
       (store default_md M_dispatch)
       (store default_od O_dispatch)
       (store default_cxd C-X_dispatch)
       (store 0 token_hackers)
       (store read_line_tg tg)
       (bind_array_cell dispatch 25 'read_line_yank)   ;25=^y
       (bind_array_cell dispatch terminator 'return_from_reader)
       (set_loc echo_buf 0)
       (delete echo_buf (length echo_buf))
       (insert prompt echo_buf)
       (set_loc echo_buf (iferror (rsearch echo_buf ":") (length echo_buf)))
       (cond
          ((eq terminator 13) (insert "(<cr>)" echo_buf))
          ((eq terminator 27) (insert "(<alt>)" echo_buf))
          ((eq terminator 32) (insert "(<sp>)" echo_buf)))
       (set_loc echo_buf (length echo_buf))
       (set_mark echo_mark echo_buf (length echo_buf))

       (errset "abort"
               ((reader echo_buf echo_screen))
               ((store 0 recursive_read_line)
                (signal "abort")))

       (store 0 recursive_read_line)
       (store (make_gnirt) tg1)
       (insert_region echo_buf (eval_mark echo_mark echo_buf)
                               (length echo_buf) tg1)
       (cat_into old_read_line tg1)
       (push tg1))

(defun read_line_yank ()
       (insert old_read_line echo_buf))

(defun print_msg (string obnox)
       (store -1 hold)
       (cond ((eq obnox 1) (print 7 60 23 0)))
       (replace message string)
       (store (or display_mode_line 1) display_mode_line))

(defun replace (gnirt string)
       (delete gnirt (length gnirt))
       (insert string gnirt))

(defun get_function (array index)
       (cond ((stringp (store (ar array index) function_to_call))
              (store (auto_load function_to_call array index)
                     function_to_call))
             ((t) (push function_to_call))))

(defun auto_load
 (string array index)
       (cat_into tg library_dir ">" string ".sine")
       (cond ((lp (load_file tg) 0) (blow_out "Auto_load Failed")))
       (as (make_variable string) array index))

(defun auto_load_rest ()
     (ift normal_aux_loaded skip_load)
     (store (t) normal_aux_loaded)
     (cat_into tg library_dir ">" "normal_aux.sine")
     (cond ((lp (load_file tg) 0) (blow_out "Normal_Aux Load Failed")))
skip_load
     (store (t) normal_aux_loading)
     (rplacd (cdr current_mode) (cons 'normal_aux_start (cddr current_mode)))
     (normal_aux_start))

(defun finish_auto_load_rest ()
     (store (nil) normal_aux_loading)
     (now_autoloading)
     (cond ((eq cmnd_1 24) (get_function C-X_dispatch cmnd_2))
           ((eq cmnd_1 27) (get_function M_dispatch cmnd_2))
           ((or (eq cmnd_1 31) (eq cmnd_1 3)) (get_function O_dispatch cmnd_2))
           ((t) (get_function dispatch cmnd_1)))
     (call_vbl function_to_call 0)
     (store 3 abort_flag))

(defun normal_aux_start ())        ;just gets us going.....

(defun now_autoloading ())         ;provided so that autoloaded
     ;procedures can be redfined by calling this after the loading has
     ;taken place but before any functions are called

(defun set_major_mode (&aux mode_name)
     (store (read_line "Major Mode Name: " 13) mode_name)
     (add_mode (make_string mode_name) 1 1))

(defun add_minor_mode (&aux mode_name)
     (store (read_line "Minor Mode Name: " 13) mode_name)
     (add_mode (make_string mode_name) 0 1))

; meanings of the warn argument to add_mode
;
; -1 use the mode in default_mode if mode not found
; 0  don't warn if mode not found
; 1  warn if mode not found

(defun add_mode (mode_name majority warn &aux mode_tunnel)
       (ift (member mode_name (car current_mode)) just_return)
       (cond ((not (member mode_name loaded_modes))
              (cond ((lp (load_file 
                          (cat_into tg library_dir \3e mode_name "_mode.sine"))
                         0)
                     (cond ((lp warn 0) (add_mode default_mode majority 1))
                           ((gp warn 0) (print_msg "Mode Not Found" 1)))
                     (goto just_return)))
              (store (cons mode_name loaded_modes) loaded_modes)))
       (store (make_variable (cat_into tg mode_name "_mode")) mode_tunnel)
       (cond ((eq majority 1)
              (replace (caar current_mode) mode_name)
              (rplaca (cdr current_mode) mode_tunnel))
             ((t)
              (rplacd (car current_mode) (cons (insert mode_name (make_gnirt))
                                               (cdar current_mode)))
              (rplacd (cdr current_mode)
                      (cons mode_tunnel (cddr current_mode)))))
       (store 3 abort_flag)
       (store -1 display_mode_line)
just_return)

(defun member (object list)
loop   (ift (eq list 0) return_nil)
       (cond ((eq object (car list)) (t))
             ((t)
              (store (cdr list) list)
              (goto loop)
return_nil    (nil))))

(defun blow_out (msg)
     (print_msg msg 1)
     (signal "abort"))

;;;  User_init is a locally bound vbl here so that other peoples' user_inits
;;;  don't get randomly and unpredictably redefined.

(defun load_file (filename &aux user_init old_user_init code)
     (store user_init old_user_init)              ; to see if redefined
     (cond ((progn (store (load filename) code)
                   (not (eq old_user_init user_init)))
            (call_vbl 'user_init 0)))
     (push code))

;(defun decode_key (string &aux n char next
;                              control_x control meta index modifier)
;    (store 0 n)
;    (store (store (store (store 0 control_x) index) control) meta)
;parse_loop
;    (store (upper_case (store (nth string n) char)) modifier)
;    (store (nth string (add n 1)) next)
;    (cond  ((eq char -1) (goto got_it))
;           ((t) (cond ((and (eq (upper_case index) \58) (eq control 1))
;                       (store 1 control_x) (store (store 0 control) index)))))
;    (cond ((eq next -1) (ifnil (eq index 0) error)
;                        (store char index) (goto got_it))
;          ((and (eq modifier \43) (eq next \2d)) (ifnil (eq control 0) error)
;                                                 (store 1 control))
;          ((and (eq modifier \4d) (eq next \2d)) (ifnil (eq meta 0) error)
;                                                 (store 1 meta))
;          ((and (eq modifier \44)
;                (and (eq (upper_case next) \45)
;                     (eq (upper_case (nth string (add n 2))) \4c)))
;           (store \7f index) (goto got_it))
;          ((eq modifier \5e) (ifnil (eq index 0) error)
;                             (store 1 control) (store next index))
;          ((t) (store char index) (store (add n 1) n) (goto parse_loop)))
;    (store (add 2 n) n)
;    (goto parse_loop)
;got_it
;    (cond ((eq index 0) (goto error)))
;    (cond ((eq control 1) (store (and index \1f) index)))
;    (cond ((eq control_x 1) (ift (eq meta 1) error)
;                            (cons C-X_dispatch index))
;          ((eq meta 1) (cons M_dispatch index))
;          ((t) (cons dispatch index))
;          ((t) error (cons 0 0))))
;
;(defun upper_case (char)
;    (cond ((and (gp char \60) (lp char \7b)) (sub char 32))
;          ((t) (push char))))

^L