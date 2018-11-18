
(documentation ">doc>sd>dired.doc")

(variable add_mode)
(variable find_file)
(variable goto_buffer)
(variable load_file)
(variable print_msg)
(variable invoke_editor)
(variable read_line)
(variable replace)
(variable replace_all_util)

(variable C-X_dispatch)
(variable default_d)
(variable default_cxd)
(variable default_md)
(variable current_buffer)
(variable current_screen)
(variable current_tvbuf)
(variable echo_buf)
(variable tg)
(variable library_dir)

(variable dir)
(variable star)
(variable dired_cte)
(variable CR_Tab)

(defun user_init()
     (store (make_gnirt) dir)
     (store (make_gnirt) star)
     (store (make_gnirt) dired_cte)
     (store "
     " CR_Tab)
     (load_file (cat_into tg library_dir ">replace.sine"))
     (as 'rcr_command C-X_dispatch 22)            ;C-X-C-V
     (as 'rcr_command default_cxd 22)             ;C-X-C-V
     (as 'dired C-X_dispatch 68)                  ;C-X-D
     (as 'dired default_cxd 68)                   ;C-X-D
     (as 'dired C-X_dispatch 100)                 ;C-X-d
     (as 'dired default_cxd 100))                 ;C-X-d

(defun dired ()
     (replace dir (read_line "Dired Directory: " 13))
     (replace star (read_line "Dired Starname: " 13))
     (dired_ dir star))

(defun dired_ (dir star &aux dired_last_buffer)
     (cond ((eq (length dir) 0)
             (insert (iferror (call_af "wd")
                              (progn (print_msg "No Wdir?" 1) (return)))
                     dir)))
     (cond ((eq (length star) 0 ) (insert "**" star)))
     (set_loc echo_buf 0)
     (delete echo_buf (length echo_buf))
     (insert "Dired Pathname: " echo_buf)
     (insert dir echo_buf)
     (insert ">" echo_buf)
     (insert star echo_buf)
     (replace dired_cte "de")
     (store (car current_tvbuf) dired_last_buffer)
     (goto_buffer (insert ".dired" dir))
     (set_loc current_buffer 0)
     (delete current_buffer (length current_buffer))
     (delete dir 6)
     (cat_into tg "entries " dir ">" star)
     (insert (iferror (call_af tg)
                      (progn (print_msg "No Such Dir?" 1)
                             (goto_buffer dired_last_buffer)
                             (return)))
             current_buffer)
     (insert 13 current_buffer)
     (set_loc current_buffer 0)
     (insert 9 current_buffer)
     (replace_all_util current_buffer 0 (length current_buffer) 32 CR_Tab)
     (add_mode "DIRED" 1 1)                       ;major mode
     (set_loc current_buffer 0)
loop
     (errset "abort" ((invoke_editor 1))          ;1 => recursive editor
                     ((goto loop)))
     (goto_buffer dired_last_buffer)
     (return))

(defun rcr_command ()
     (replace tg (read_line "Recursive File Name: " 13))
     (rcr tg))

(defun rcr (file &aux last_dired_buffer)
     (store (car current_tvbuf) last_dired_buffer)
     (find_file file)
loop
     (errset "abort" ((invoke_editor 1))          ;1 => recursive editor
                     ((goto loop)))
     (goto_buffer last_dired_buffer))
wotT (31.672 27 2.21) >s>editors>tv

^P^X
