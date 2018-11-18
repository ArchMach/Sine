
(variable dispatch M_dispatch O_dispatch C-X_dispatch)
(variable current_screen echo_buf echo_screen doc_buf doc_window doc_file)
(variable hold replace tg)

(documentation ">doc>sm>print_doc.doc")

(defun print_doc ( &aux doc fun current_screen first )
       (store echo_screen current_screen)
       (set_loc echo_buf 0)
       (delete echo_buf (length echo_buf))
       (insert "TV Command: " echo_buf)
       (cond ((eq (tyis) 0) (display)))
       (store (tyi) first)
       (cond ((eq first 27)
              (insert "Meta " echo_buf)
              (cond ((eq (tyis) 0) (display)))
              (store (ar M_dispatch (insert_rep (tyi))) fun))
             ((eq first 24)
              (insert "C-X " echo_buf)
              (cond ((eq (tyis) 0) (display)))
              (store (ar C-X_dispatch (insert_rep (tyi))) fun))
             ((eq first 3)
              (insert "Ortho " echo_buf)
              (cond ((eq (tyis) 0) (display)))
              (store (ar O_dispatch (insert_rep (tyi))) fun))
             ((eq first 31)
              (insert "Para " echo_buf)
              (cond ((eq (tyis) 0) (display)))
              (store (ar M_dispatch 32) fun))          ;known illegal_command
;             (store (ar O_dispatch (insert_rep (tyi))) fun))
             ((t) (store (ar dispatch first) fun)
              (insert_rep first)))
       (cond ((eq (tyis) 0) (display)))
       (cond ((stringp fun)
              (print_clearing (cat_into tg "Auto_Loaded Function '" fun "'")
                              1 1 1)
              (store 1 hold))
             ((t)
              (cond ((functionp (store (get_documentation fun) doc))
                     (+push fun)
                     (call_vbl doc 1))
                    ((t)
                     (cond ((eq doc_buf 0)
                            (store (make_buffer) doc_buf)
                            (store (make_window doc_buf) doc_window)
                            (store (make_gnirt) doc_file)))
                     (cond ((not (eq doc doc_file))
                            (read_file doc_buf doc)
                            (replace doc_file doc)))
                     (print_clearing (find_doc (get_pname fun) doc_buf) 1 1 1)
                     (store 1 hold))))))

(defun find_doc (string buf &aux start stop)
       (cat_into tg 4 string ":")
       (set_loc buf 0)
       (store (sub (iferror (search buf tg)
                            (progn (+push
                                    (cat_into tg
                                              "No Documentation On " string))
                                   (return)))
                   (sub (length tg) 1)) start)
       (set_loc buf start)
       (define_window (cond ((eq buf doc_buf) (push doc_window))
                            ((t) (make_window buf)))
                      start
                      (iferror (sub (search buf 4) 1) (length buf))))

(defun insert_rep (char)
       (cond ((eq char 27) (insert "Alt" echo_buf))
             ((lp char 32)
              (insert "C-" echo_buf)
              (insert (add char 64) echo_buf))
             ((eq char 32) (insert "Space" echo_buf))
             ((eq char 127) (insert "Del" echo_buf))
             ((t) (insert char echo_buf)))
       (push char))

^L