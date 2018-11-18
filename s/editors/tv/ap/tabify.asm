
(documentation ">doc>sm>tabify.doc")

(variable argument argumentp current_buffer current_mark end_of_line)

;Tabify Region, turning spaces into tabs
;    region is defined by the mark and current_location
;    argument is tab spacing
;    no argument, default to 5

(defun tabify (&aux end need)
     (store (set_up_for_region) end)
     (do_while (gep end (location current_buffer))
          (set_loc current_buffer
               (iferror (search current_buffer "  ") (goto exit)))
          (cond ((gep (location current_buffer) end) (goto exit)))
          (add_to_loc current_buffer -2)
          (store (sub argument (mod (sub (get_hpos_varying current_buffer
               (location current_buffer) argument) 1) argument)) need)
          (cond
               ((gep (find_first_not_in_fr current_buffer " ") need)
                    (insert 9 current_buffer)
                    (delete current_buffer need)
                    (store (sub end (sub need 1)) end))
               ((t)
                    (set_loc current_buffer
                         (find_first_not_in_fa current_buffer " ")))))
     exit
     (set_loc current_buffer end)
     (store 0 argument))


;does a get_hpos with different tab spacings

(defun get_hpos_varying (buffer location tab_spacing &aux i hpos count)
     (store (sub 0 (iferror (rsearchr current_buffer 13)
          (location current_buffer))) i)
     (store 1 hpos)
     (do_while (lp i 0)
          (cond
               ((eq 9 (nthr current_buffer i))
                    (store (sub argument (mod (sub hpos 1) argument)) count))
               ((t) (store 1 count)))
          (store (add count hpos) hpos)
          (store (add 1 i) i))
     (return_value hpos))


;Untabify Region, turning tabs into spaces
;    region is defined by the mark and current_location
;    argument is tab spacing
;    no argument, default to 5

(defun untabify (&aux dist end)
     (store (set_up_for_region) end)
     (do_while (gep end (location current_buffer))
          (set_loc current_buffer (iferror
               (search current_buffer 9) (goto exit)))
          (cond ((gep (location current_buffer) end) (goto exit)))
          (add_to_loc current_buffer -1)
          (store (sub argument (mod (sub (get_hpos current_buffer
               (location current_buffer)) 1) argument)) dist)
          (delete current_buffer 1)
          (store (add end (sub dist 1)) end)
          (do_while (gp dist 0)
               (insert 32 current_buffer)
               (store (sub dist 1) dist)))
     exit
     (set_loc current_buffer end)
     (store 0 argument))


;initializer for both region oriented functions

(defun set_up_for_region (&aux loc_mark end)
     (cond ((not argumentp) (store 5 argument)))
     (store (eval_mark current_mark current_buffer) loc_mark)
     (cond
          ((gp loc_mark (location current_buffer))
               (store loc_mark end))
          ((t)
               (store (location current_buffer) end)
               (set_loc current_buffer loc_mark)))
     (return_value end))

; Trim_White deletes all trailing whitespace from a region
; (because it is line oriented, its boundaries are only approxomately
;  those of the point and mark)

(defun trim_white (&aux end end_mark)
     (store (set_up_for_region) end)
     (store (make_mark) end_mark)
     (set_mark end_mark current_buffer end)
     (do_while (lp (location current_buffer)
                   (eval_mark end_mark current_buffer))
          (end_of_line)
          (delete current_buffer
               (iferror (find_first_not_in_br current_buffer "   ") 0))
          (add_to_loc current_buffer 1))
     (unset_mark end_mark current_buffer))
^L