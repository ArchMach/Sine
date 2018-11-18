
(variable current_buffer current_filename current_tvbuf echo_buf echo_screen)
(variable dispatch)
(variable redo_mode_line read_line replace goto_buffer add_mode
          over_s_expr_fr back_word return_from_reader load_file
          self_insert_break invoke_editor save_changes next_line
          blow_out print_msg get_meta_cmd)
(variable tg alphabetics white_space display_mode_line hold)
(variable find_doc)

(variable current_info_node extra_info)
(variable completed comp_search filename menu_items)
(variable info_new_buf info_node_list starting_node info_history)
(variable completing_start info_previous_node)

(documentation ">u>ota>sine>p>info.doc")

(defun user_init ( &aux root_buffer root_fid )
       (read_file (store (make_buffer) root_buffer)
                  (store ">u>ota>sine>p>root_node" root_fid))
       (store (list ">" root_fid root_buffer) starting_node)
       (store (store (nil) current_info_node) info_node_list)
       (store (make_buffer) extra_info)
       (store (make_gnirt) completed)
       (store (make_gnirt) comp_search)
       (store (make_gnirt) filename)
       (store (make_gnirt) menu_items)
       (cond ((eq find_doc 0) (load_file ">sl1>sinemacs>print_doc.sine"))))

;    The format of an info_node is as follows:
;         (node_name filename_of_node_info
;               buffer_containing_file
;               child1 child2 ... childn)

(defun get_info_node (item file &aux new_node buf)
       (cond ((store (find_info_node item file) new_node)
              (cond ((eq (car new_node) item)
                     (return_value new_node))
                    ((return_value
                      (make_new_node item file (caddr new_node))))))) ;buffer
                     
(print_clearing "reading in" 1 2 0)
(print file 12 2 0)

       (cond ((lp (read_file (store (make_buffer) info_new_buf) file) 0)
              (print "failed" 0 0 0) (nil))
             ((t) (make_new_node item file info_new_buf))))

(defun find_info_node (item file &aux this_node ff info_node_list)
       (store (nil) ff)
       (do_while info_node_list
                 (store (car info_node_list) this_node)
                 (cond ((eq file (cadr this_node))
                        (cond ((eq item (car this_node))
                               (return_value this_node))
                              ((store this_node ff)))))
                 (store (cdr info_node_list) info_node_list))
       (push ff))        ;return filename or nil if no real matches found

(defun make_new_node (item file buf &aux node)
       (store (cons (store (list (replace (make_gnirt) item)
                                 (replace (make_gnirt) file) buf)
                           node)
                    info_node_list)
              info_node_list)
       (push node))

(defun info ( &aux buffer_to_return_to )
       (store (nil) info_history)
       (store starting_node current_info_node)
       (store (car current_tvbuf) buffer_to_return_to)
       (goto_buffer "INFO")
       (add_mode "info" 1 1)
       (install_new_node)
       (errset "abort" ((select_child_node)) ())
loop   (errset "abort" ((invoke_editor 1))
               ((goto loop)))
       (goto_buffer buffer_to_return_to))

(defun install_new_node ( &aux info_topic end_extra )
       (store (car current_info_node) info_topic)
       (replace current_filename info_topic)
       (set_loc current_buffer 0)
       (delete current_buffer (length current_buffer))
       (insert (find_doc info_topic (caddr current_info_node)) current_buffer)
       (set_loc current_buffer 0)
       (set_loc extra_info 0)
       (delete extra_info (length extra_info))
       (insert_region current_buffer
                      (set_loc current_buffer
                               (find_first_in_fa current_buffer \28))
                      (add (location current_buffer)
                           (store (over_s_expr_fr current_buffer) end_extra))
                      extra_info)
       (delete current_buffer end_extra)
       (store -1 display_mode_line))

(defun select_child_node ( &aux item)
       (cond ((store (read_child_node) item)
              (cond ((eq (goto_child_node item) 0)
                     (blow_out "Completion lossage"))))))

(defun read_child_node ( &aux item self_insert_break )
       (store try_completing self_insert_break)
       (set_loc current_buffer 0)
       (delete current_buffer (length current_buffer))
       (set_loc extra_info 0)
       (store (set_loc extra_info (iferror (search extra_info \28)
                                           (blow_out "Illegal node format")))
              completing_start)

;collect together all the possible menu items so we can show him his choices.
; The form is of a list of lists where the forst token in each list is
; the name of the menu item.

       (do_until (eq (nthr extra_info 0) \29)
                 (set_loc extra_info (find_first_not_in_fa extra_info
                                                           white_space))
                 (insert_region extra_info
                                (add (location extra_info) 1)    ;skip the open
                                (find_first_in_fa extra_info white_space)
                                current_buffer)
                 (insert 13 current_buffer)       ;CR between each
                 (set_loc extra_info (search extra_info \29)))

       (errset "abort" ((return_value (read_line "Menu Item: " 13)))
                       ((install_new_node) (return_value (nil)))))    ;cleanup

(defun goto_child_node (item &aux first_child new_node )
       (set_loc extra_info 0)
       (set_loc extra_info
                (iferror (search extra_info (cat_into tg \28 item " "))
                         (return_value -1)))
       (delete filename 10000)
       (insert_region extra_info
                      (set_loc extra_info
                               (find_first_not_in_fa extra_info white_space))
;(  To balance the quoted close paren
                      (set_loc extra_info (find_first_in_fa extra_info ")/"))
                      filename)
       (cond ((eq (nthr extra_info 0) "/")
              (delete tg 10000)
              (store (insert_region extra_info
                                    (add (location extra_info) 1)
                                    (find_first_in_fa extra_info \29)
                                    tg)
                     item)))
       (store (cdddr current_info_node) first_child)
       (do_while first_child
                 (cond ((eq item (caar first_child))
                        (store (car first_child) new_node)
                        (goto got_it)))
                 (store (cdr first_child) first_child))

;;;  The node does not already exist so create it and thread it in.

       (store (get_info_node item filename) new_node)
       (rplacd (cddr current_info_node)
               (cons new_node (cdddr current_info_node)))
got_it
       (push_current_info_node)
       (store new_node current_info_node)
       (install_new_node))

(defun try_completing ()
       (delete tg 10000)
       (back_word)
       (insert_region current_buffer
                      (location current_buffer)
                      (length current_buffer)
                      tg)
       (cond ((lep (get_whole_string tg extra_info
                                     completing_start \28 completed)
                   0)
              (tyo 7) (set_loc current_buffer (length current_buffer)))
             ((t)
              (delete current_buffer (sub (length current_buffer)
                                          (location current_buffer)))
              (insert completed current_buffer)
              (return_from_reader))))

(defun get_whole_string (attempt completion_table start prefix answer)
       (cat_into comp_search prefix attempt)
       (set_loc completion_table start)
       (set_loc completion_table
                (iferror (search completion_table comp_search)
                         (return_value -1)))
       (iferror (search completion_table comp_search)
                (add_to_loc completion_table (sub 0 (length attempt)))
                (delete answer 10000)
                (insert_region completion_table
                               (location completion_table)
                               (find_first_in_fa completion_table white_space)
                               answer)
                (return_value 1))
       (return_value 0))

(defun push_current_info_node ()
       (store (cons current_info_node info_history) info_history))

(defun maybe_display ()
       (cond ((eq (tyis) 0) (display))))

(defun peruse_history ( &aux info_history current_screen )
       (store echo_screen current_screen)
       (do_while info_history
                 (set_loc echo_buf 0)
                 (delete echo_buf (length echo_buf))
                 (insert (caar info_history) echo_buf)
                 (maybe_display)
                 (cond ((eq (tyi) "y")
                        (print_msg "Yes" 0)
                        (return_value (car info_history))))
                 (store (cdr info_history) info_history))
       (print_msg "No Previous Node" 1)
       (nil))

(defun back_up ( &aux new_node )
       (cond ((store (peruse_history) new_node)
              (push_current_info_node)
              (store new_node current_info_node)
              (install_new_node))))

(defun info_make_global ( &aux known_under buf item )
       (store (read_line "What should this node be know as? " 13) known_under)
       (store (car current_info_node) item)
       (cond ((eq (length known_under) 0) (store item known_under)))
       (store (caddr starting_node) buf)
       (set_loc buf 0)
       (iferror (search buf (cat_into tg \28 known_under " "))
                (set_loc buf (search buf \28))
                (insert 13 buf)
                (insert \28 buf)
                (insert known_under buf)
                (insert " " buf)
                (insert (cadr current_info_node) buf)
                (cond ((not (eq known_under item))
                       (insert  "/" buf)
                       (insert item buf)))
                (insert \29 buf)
                (write_file buf (cadr starting_node))
                (return))
       (blow_out "That global node exists"))

(defun edit_node ( &aux save_changes buf )
       (store current_buffer buf)
       (goto_buffer "edit_info")
       (add_mode "doc" 1 1)
       (set_loc current_buffer 0)
       (delete current_buffer (length current_buffer))
       (insert buf current_buffer)
       (set_loc current_buffer (location buf))
       (store update_info save_changes)
loop   (errset "abort" ((invoke_editor 1))          ;1 => recursive editor
                       ((goto loop)))
       (store current_buffer buf)
       (goto_buffer "INFO")
       (set_loc current_buffer 0)
       (delete current_buffer (length current_buffer))
       (insert buf current_buffer)
       (set_loc current_buffer (location buf)))

(defun update_info ( &aux buf temp )
       (store (caddr current_info_node) buf)
       (cat_into tg 4 (car current_info_node) ":")
       (set_loc buf 0)
       (set_loc buf (search buf tg))
       (add_to_loc buf (over_s_expr_fr buf))
       (delete buf (iferror (sub (searchr buf 4) 1)
                            (sub (length buf) (location buf))))
       (store (location current_buffer) temp)
       (set_loc current_buffer 0)
       (next_line)
       (insert_region current_buffer
                      (location current_buffer)
                      (length current_buffer)
                      buf)
       (set_loc current_buffer temp)
       (write_file buf (cadr current_info_node))
       (print_msg "Updated" 0))

(defun add_child ( &aux new_node buf get_meta_cmd )
       (store add_previous get_meta_cmd)
       (store 0 info_previous_node)
       (replace tg (read_line "Menu Name: " 13))
       (cond ((eq info_previous_node 0)
              (replace filename (read_line "What File is it in? " 13))
              (cond ((not (eq info_previous_node 0))
                     (store info_previous_node new_node)
                     (goto got_node)))
              (cond ((eq (length filename) 0)     ;use the current filename
                     (insert (cadr current_info_node) filename)))
              (replace menu_items (read_line "What is the Node's Name? " 13))
              (cond ((eq (length menu_items) 0) (insert tg menu_items)))
              (cond ((not (store (get_info_node menu_items filename) new_node))
                     (store (make_new_node menu_items filename info_new_buf)
                            new_node)))
              (cat_into completed 4 menu_items ":")
              (store (caddr new_node) buf)
              (set_loc buf 0)
;;;add to file if not already there
              (iferror (search buf completed)
                       (insert completed buf)
                       (insert "
()
" buf)
                       (cond ((lp (write_file buf filename) 0)
                              (blow_out "Can't update info")))))
;;; He specified a previously visited node
             ((t)
              (store info_previous_node new_node)
              (replace tg (car new_node))
got_node
              (replace menu_items (car new_node))))

       (store (caddr current_info_node) buf)
       (cat_into completed 4 (car current_info_node) ":")
       (set_loc buf 0)
       (set_loc buf (search buf completed))
       (cat_into filename \28 tg " ")   ;so we can search for dups.
       (cond ((lp (iferror (search buf filename) (length buf))
                  (iferror (search buf 4) (length buf)))
              (blow_out "That Menu item exists")))
       (set_loc buf (search buf \28))
       (cat_into completed 13 \28 tg " " (cadr new_node))
       (cond ((not (eq tg menu_items))
              (insert "/" completed)
              (insert menu_items completed)))
       (insert \29 completed)
       (insert completed buf)
       (write_file buf (cadr current_info_node))
       (rplacd (cddr current_info_node)           ;thread it in
               (cons new_node (cdddr current_info_node)))
       (install_new_node))

(defun add_previous ( &aux temp )
       (cond ((store (peruse_history) temp)
              (store temp info_previous_node)
              (return_from_reader))))

(defun change_menu ())^L 
^