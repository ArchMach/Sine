
(variable current_buffer)
(variable current_tvbuf)
(variable current_mark)
(variable switch_point_and_mark)
(variable forward_s_expr)
(variable replace)
(variable full_redisplay)
(variable pagel)
(variable db_g)

(defun user_init ()
       (store (make_gnirt) db_g))

(defun try_fun ( &aux fun com s e fid )
     (cond ((lp (eval_mark current_mark current_buffer)
                (location current_buffer))
            (switch_point_and_mark)))
     (store (define_window (make_window current_buffer)
               (location current_buffer)
               (eval_mark current_mark current_buffer))
            fun)
     (write_file fun (store
               (call_af "cat >pd>ss> [user id] . [short_date] .asm")
               fid))
     (cat_into db_g "lisp asm " fid)
     (delete db_g 4)
     (cline db_g)
     (delete (replace db_g fid) 4)
     (load (insert ".sine" db_g))
     (full_redisplay))

(defun print_number (msg place num &aux mask )
       (display_screen 0 (sub pagel 2) 1)         ;clear an area to use
       (print msg place (sub pagel 2) 0)
       (tyo ":")
       (delete db_g 10000)
       (print (insert_ioa "^i" num db_g) 0 0 0))

(defun print_object (msg place num &aux mask )
       (display_screen 0 (sub pagel 2) 1)         ;clear an area to use
       (print msg place (sub pagel 2) 0)
       (tyo ":")
       (delete db_g 10000)
       (print (insert_ioa "^8w" num db_g) 0 0 0))
^L