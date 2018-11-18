
(documentation ">doc>sm>ascii.doc")

(variable current_buffer)

(defun ascii_chart(&aux index)
       (store 0 index)
loop
       (ascii_put index (t))
       (ascii_put (add index 32) (t))
       (ascii_put (add index 64) (t))
       (ascii_put (add index 96) (nil))
       (insert 13 current_buffer)
       (store (add index 1) index)
       (ift (lp index 32) loop))

(defun ascii_put(char blanks)
       (insert_ioa " ^3d  " char current_buffer)
       (insert_ioa " ^2w  " char current_buffer)
       (cond ((lp char 32)
              (insert "^" current_buffer)
              (insert (add char 64) current_buffer)
              (ifnil blanks punt)
              (insert "        " current_buffer))
             ((eq char 127)
              (insert "DEL" current_buffer)
              (ifnil blanks punt)
              (insert "       " current_buffer))
             ((t)
              (insert char current_buffer)
              (ifnil blanks punt)
              (insert "          " current_buffer)) )
punt   )

       
^L