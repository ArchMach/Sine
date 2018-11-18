
(variable hack_window)
(variable linel)
(variable screen_length)
(variable current_screen)
(variable char)
(variable buf)
(variable mark)
(variable dispatch_table)
(variable echo_buf)
(variable echo_area)
(variable echo_mark)
(variable buffer_stack)
(variable search_string_hack)
(variable saved_search_string)

(defun top_level ( &aux main_buffer screen )
     (store (cons (store (make_buffer) main_buffer) 0) buffer_stack)
     (store (make_marker) mark)
     (store (make_screen main_buffer 1 21) screen)
     (store 80 linel)
     (store 21 screen_length)
     (store (make_array 32 128) dispatch_table)   ; make a FW array for dispatching
     (store (make_screen (store (make_buffer) echo_buf) 24 1) echo_area)
     (store (make_marker) echo_mark)
     (store (make_window main_buffer) hack_window)
     (store (make_window echo_buf) search_string_hack)
     (store (make_gnirt) saved_search_string)

     (fill_vbl_array dispatch_table 128
do_set_mark beginning_of_line do_back do_return        ;^@
do_delete end_of_line do_forward print_test            ;^D
do_back insert_char pp_buffers kill_line               ;^H
full_redisplay carriage_return next_line insert_char   ;^L
previous_line insert_char input_file string_search     ;^P
top_of_buffer insert_char next_page output_file        ;^T
insert_char illegal_char bottom_of_buffer alt_mode     ;^X
insert_char insert_char insert_char insert_char        
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
backward_token insert_char forward_token insert_char   ;<
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char insert_char
insert_char insert_char insert_char do_rubout
)
reader_loop
     (reader main_buffer screen)
     (return)
     (goto reader_loop))

(defun reader (buf current_screen)
     (full_redisplay)
     (errset "^opcode "                 ;this is just a hack to non-local goto
             (loop  (ifne (tyis) 0 just_read)
                    (display)
              just_read
                    (call_vbl (ar dispatch_table (store (tyi) char)) 0)
                    (goto loop))
             ((return))))

(defun forward_token ()
     (set_loc buf (over_token_fa buf)))

(defun backward_token ()
     (set_loc buf (over_token_ba buf)))

(defun print_test ()
     (define_window hack_window (location buf) (add (location buf) 5))
     (print hack_window 70 5)
     (tyi)
     (return))

(defun insert_char ()
     (insert char buf))

(defun carriage_return ()
     (insert 13 buf))

(defun alt_mode ()
     (insert 27 buf))

(defun illegal_char ()
     (insert 1 current_screen))    ;should blow out with a bad type error

(defun do_set_mark ()
     (set_mark mark buf (location buf)))

(defun do_delete ()
     (delete buf 1))

(defun do_rubout ()
     (delete buf -1))

(defun next_line ( &aux offset )
     (store (sub -1 (iferror (rsearchr buf 13)
                             (sub -1 (location buf)))) offset)
     (set_loc buf (iferror (search buf 13) (return)))
     (add_to_loc buf (min offset
                          (sub (iferror (searchr buf 13)
                                        (add (sub (length buf)
                                                  (location buf)) 1)) 1))))

(defun next_page ( &aux i )
     (store (sub screen_length 2) i)
next_page_loop
     (ife i 0 quit)
     (next_line)
     (store (sub i 1) i)
     (goto next_page_loop)
quit (force_display current_screen 1))

(defun previous_line ( &aux offset )
     (store (sub -1 (iferror (rsearchr buf 13) (return))) offset)
     (add_to_loc buf (sub -1 offset))
     (set_loc buf (iferror (add 1 (rsearch buf 13)) (push 0)))
     (add_to_loc buf (min offset (sub (searchr buf 13) 1))))

(defun pp_buffers (&aux new_buf)
     (ife (tyi) 0 pop)
     (store (cons (store (make_buffer) new_buf) buffer_stack) buffer_stack)
     (store new_buf buf)
     (return)
pop  (store (cdr buffer_stack) buffer_stack)
     (store (car buffer_stack) buf))

(defun beginning_of_line ()
     (set_loc buf (add (iferror (rsearch buf 13) (push -1)) 1)))

(defun end_of_line ()
     (set_loc buf (sub (search buf 13) 1)))

(defun kill_line ( &aux ll )
     (store (sub (searchr buf 13) 1) ll)
     (delete buf (max 1 ll)))

(defun do_return ()                ;the reader handles II and returns
     (number 0))

(defun full_redisplay ()
     (force_display current_screen -1))

(defun do_switch_pm ( &aux temp_loc )
     (store (location buf) temp_loc)
     (set_loc buf (eval_mark mark buf))
     (set_mark mark buf temp_loc))

(defun do_back ()
     (add_to_loc buf -1))

(defun do_forward ()
     (add_to_loc buf 1))

(defun input_file ()
     (read_file buf (read_line ifn 13))
     (full_redisplay)
     (return)
ifn  (string "input file name: "))

(defun output_file ()
     (write_file buf (read_line ofn 13))
     (return)
ofn  (string "output file name: "))

(defun top_of_buffer ()
     (set_loc buf 0))

(defun bottom_of_buffer ()
     (set_loc buf (length buf)))

(defun string_search ( &aux ss )
;    (store do_return alt_mode)
;    (set_loc echo_buf 0)
;    (delete echo_buf (length echo_buf))
;    (reader echo_buf echo_area)
     (store (read_line ssp 27) ss)
     (ife (length ss) 0 do_search)
; he gave us a new search string
     (delete saved_search_string (length saved_search_string))
     (insert ss saved_search_string)
do_search
     (set_loc buf (iferror (search buf saved_search_string) (goto sfailed)))
     (return)
sfailed
     (print fsp 20 24)
     (return)
fsp  (string "Search Failed")
ssp  (string "Search String: "))

;    useful routines

(defun read_line (prompt terminator )
     (bind_array_cell_to dispatch_table terminator (quote do_return))
     (set_loc echo_buf 0)
     (delete echo_buf (length echo_buf))
     (insert prompt echo_buf)
     (set_mark echo_mark echo_buf (length echo_buf))

     (reader echo_buf echo_area)
     (push (define_window search_string_hack
                          (eval_mark echo_mark echo_buf)
                          (length echo_buf))))

^L