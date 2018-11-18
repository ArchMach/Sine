
(variable argument cmnd_1 keep_on_tunneling self_insert current_buffer)
(static flag)

(documentation ">doc>sm>overwrite.doc")

(defun user_init()
     (store (t) flag))

(defun overwrite_mode ()
     (keep_on_tunneling))

(defun overwrite ()
     (cond ((or (eq argument 4) (eq argument -1))
               (store (nil) flag))
           ((t)
               (store (t) flag)))
     (store 0 argument))

;    overwrite mode does replacing instead of inserting for most characters

(defun self_insert ()
     (ifnil flag skip)
     (cond
          ((eq (nthr current_buffer 0) 9)
               (cond ((eq 4 (mod (sub (get_hpos current_buffer
                                   (location current_buffer)) 1) 5))
                         (delete current_buffer 1))))
          ((not (eq (nthr current_buffer 0) 13))
               (delete current_buffer 1)))
skip
     (insert cmnd_1 current_buffer))
^L