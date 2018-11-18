
;;; REPLACE PACKAGE

(documentation ">doc>sm>replace.doc")

(variable print_msg)
(variable read_line)
(variable replace)

(variable current_buffer)
(variable M_dispatch)
(variable default_d)
(variable default_cxd)
(variable default_md)
(variable current_modifiedp)

(static tg1)
(static tg2)

(defun user_init()
       (store (make_gnirt) tg1)
       (store (make_gnirt) tg2)
       (as 'global_replace M_dispatch 82)    ; M-R
       (as 'global_replace default_md 82)    ; M-R
       (as 'global_replace M_dispatch 114)   ; M-R
       (as 'global_replace default_md 114)   ; M-R
       (as 'query_replace M_dispatch 18)     ; C-M-R
       (as 'query_replace default_md 18))    ; C-M-R

(defun query_replace (&aux oldlen char oldpos)
       (read_old_new)
       (store (location current_buffer) oldpos)
       (store (length tg1) oldlen)
loop
       (set_loc current_buffer (iferror (search current_buffer tg1)
                                        (progn (print_msg "No More Matches" 1)
                                               (goto just_return))))
       (print "-replace-" 60 23 0)
       (display)
       (cond ((eq (store (tyi) char) 32)
              (store (t) current_modifiedp)
              (delete current_buffer (sub 0 oldlen))
              (insert tg2 current_buffer))
             ((eq char 7)
              (progn (print_msg "Aborting" 1)
                     (goto just_return))))
       (goto loop)
just_return
       (set_loc current_buffer oldpos))

(defun global_replace(&aux oldpos)
       (read_old_new)
       (store (location current_buffer) oldpos)
       (replace_all_util current_buffer
                         (location current_buffer)
                         (length current_buffer)
                         tg1
                         tg2)
       (set_loc current_buffer oldpos))

(defun read_old_new ()
       (replace tg1 (read_line "Old String: " 27))
       (replace tg2 (read_line "New String: " 27)))

(defun replace_all_util (buffer start end old new &aux next)
       (set_loc buffer start)
loop
       (store (iferror (search buffer old) (return)) next)
       (ifnil (lep next end) just_return)
       (set_loc buffer next)
       (delete buffer (sub 0 (length old)))
       (insert new buffer)
       (store (add (sub (length new) (length old)) end) end)
       (goto loop)
just_return)
^L