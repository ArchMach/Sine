
(variable current_buffer dispatch)
(variable punt_changes)

(defun user_init ()
       (as 'punt_changes dispatch 3)    ;for debugging
       (as 'twiddle dispatch 20))       ;C-T

(defun twiddle ( &aux point temp )
       (ift (eq (location current_buffer) 0) just_return)
       (cond ((eq (iferror (searchr current_buffer 13) 1) 1)
              (add_to_loc current_buffer -1)))
       (store (nthr current_buffer -1) temp)
       (delete current_buffer -1)
       (add_to_loc current_buffer 1)
       (insert temp current_buffer)
just_return)
^L