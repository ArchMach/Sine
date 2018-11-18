
(variable current_buffer echo_buf)

(defun status ()
       (set_loc echo_buf 0)
       (delete echo_buf (length echo_buf))
       (insert_ioa "point=^i" (location current_buffer) echo_buf)
       (insert_ioa " length=^i" (length current_buffer) echo_buf)
       (insert_ioa " (^i%)" (div (mul (location current_buffer) 100)
                                 (length current_buffer)) echo_buf))

^L