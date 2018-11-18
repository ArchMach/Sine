
(documentation ">doc>sm>pl1_mode.doc")

(static pl1_start_column)
(static pl1_continuation_column)
(static pl1_indentation)
(static pl1_undent_end)
(static pl1_declaration_hack)

(variable dispatch M_dispatch O_dispatch)
(variable current_buffer current_filename save_buffer)
(variable tg1)
(static pl1_eob)
(static pl1_sob)
(static pl1_type)
(variable beginning_of_line)
(variable next_line_command)
(variable previous_line_command)
(variable argument)
(static PLI_Breaks)
(static PLI_White)
(variable keep_on_tunneling)

(defun user_init()
       (store 0 pl1_declaration_hack)
       (store 0 pl1_start_column)
       (store 20 pl1_continuation_column)
       (store 5 pl1_indentation)
       (store 1 pl1_undent_end)
       (store (make_gnirt) tg1)
       (store (fill_char_array (make_array 1 128)
                               "+-/*:;^~=|()[]&'<>     ^L") PLI_Breaks)
       (store (fill_char_array (make_array 1 128)
                               "   ^L") PLI_White)
       (as 1 PLI_Breaks 13)
       (as 1 PLI_White 13)
       (as 1 PLI_Breaks 34))

(defun pl1_mode ()
       (bind_key C-M-P balance_backward)
       (bind_key C-M-A start_of_statement)
       (bind_key C-M-N balance_forward)
       (bind_key O-l goto_line_n)
       (bind_key C-M-M next_non_white)
       (bind_key C-M-I pl1_indent_line)
       (bind_key C-M-Z compile_pl1)
       (bind_key C-J pl1_new_line)
       (pl1_scan_options)
       (keep_on_tunneling))


(defun pl1_new_line()
       (insert 13 current_buffer)
       (pl1_indent_line))

(defun pl1_scan_options(&aux number char old)
       (store (location current_buffer) old)
       (set_loc current_buffer 0)
       (set_loc current_buffer (iferror (search current_buffer "/* options:: ")
                                        (goto noopt)))
       (store 0 number)
loop   (store (nthr current_buffer 0) char)
       (cond ((and (gep char 48) (lep char 57))
              (store (add (mul number 10) (and char 15)) number))
             ((eq char 94)
              (store 1 number))
             ((eq char 105)
              (store number pl1_indentation)
              (store 0 number))
             ((eq char 99)
              (store number pl1_continuation_column)
              (store 0 number))
             ((eq char 100)
              (store number pl1_declaration_hack)
              (store 0 number))
             ((eq char 117)
              (store number pl1_undent_end)
              (store 0 number))
             ((eq char 115)
              (store number pl1_start_column)
              (store 0 number))
             ((t)
noopt         (set_loc current_buffer old)
              (return)))
       (add_to_loc current_buffer 1)
       (goto loop))

(defun compile_pl1()
       (save_buffer)
       (delete tg1 99999)
       (insert "pl1 " tg1)
       (insert current_filename tg1)
       (cline tg1))
             
(defun next_non_white()
scan   (set_loc current_buffer
               (iferror (find_first_not_in_fa current_buffer PLI_White)
                        (length current_buffer)))
       (store (eq (location current_buffer) (length current_buffer)) pl1_eob)
       (ift pl1_eob punt)
       (ift (eq (nthr current_buffer 0) 47) com1)
punt   (return)
com1   (ifnil (eq (nthr current_buffer 1) 42) punt)
       (set_loc current_buffer (iferror (search current_buffer "*/")
                                        (length current_buffer)))
       (goto scan))


(defun next_pl1_token(&aux char stok)
       (delete tg1 (length tg1))
       (next_non_white)
       (ift pl1_eob punt)
       (store (location current_buffer) stok)
       (store (nth current_buffer stok) char)
       (ift (eq char 34) quote)
       (ift (eq (ar PLI_Breaks char) 0) snarf)
       (add_to_loc current_buffer 1)
       (insert char tg1)
punt   (return)
quote  (add_to_loc current_buffer 1)
       (set_loc current_buffer (iferror (search current_buffer 34)
                                        (length current_buffer)))
       (goto common)
snarf
       (set_loc current_buffer (over_token_fa current_buffer))
common
       (insert_region current_buffer stok (location current_buffer) tg1)
       )

(defun statement_type(&aux char)
loop   (next_pl1_token)
       (ift pl1_eob punt)
       (next_non_white)
       (ift pl1_eob punt)
       (store (nthr current_buffer 0) char)
       (ift (eq char 58) label)    ; :
       (ift (eq char 61) simple)   ; =
       (ift (eq char 46) simple)   ; .
       (ift (eq char 45) simple)   ; -
       (ift (eq tg1 "end") close)
       (ift (eq tg1 "do") open)
       (ift (eq tg1 "proc") open)
       (ift (eq tg1 "procedure") open)
       (ift (eq tg1 "begin") open)
       (ift (eq tg1 "else") else)
       (ift (eq tg1 "then") else)
       (ift (eq tg1 "if") if)
       (ift (eq tg1 "on") on)
simple (store 0 pl1_type)
skip   (set_loc current_buffer (iferror (search current_buffer 59)
                                        (length current_buffer)))
       (return)
label  (next_pl1_token)
       (ifnil pl1_eob loop)
punt   (store -1 pl1_type)
open   (store 1 pl1_type)
       (goto skip)
close  (store 2 pl1_type)
       (goto skip)
else   (store 3 pl1_type)
       (return)
if     (store 4 pl1_type)
ifloop (next_pl1_token)
       (ift pl1_eob punt)
       (ift (eq tg1 ";") punt)
       (ifnil (eq tg1 "then") ifloop)
       (return)
on     (store 5 pl1_type)
       (next_pl1_token))


(defun balance_forward(&aux depth)
       (store 0 depth)
loop                               ; 1st scan for open statement
       (statement_type)
       (ift pl1_eob punt)
       (ift (eq pl1_type 1) scan)
       (ift (eq pl1_type 2) punt)
       (goto loop)
scan                               ; 2nd scan for balancing end
       (statement_type)
       (ift pl1_eob punt)
       (ift (eq pl1_type 1) open)
       (ift (eq pl1_type 2) close)
       (goto scan)
open
       (store (add depth 1) depth)
       (goto scan)
close
       (ift (lp depth 1) punt2)
       (store (sub depth 1) depth)
       (goto scan)
punt2
       (previous_statement_cmd)
punt)


(defun start_of_statement()
       (set_loc current_buffer (iferror (rsearch current_buffer 59)
                                        (add 0 0)))
       (add_to_loc current_buffer 1)
       (next_non_white))


(defun previous_statement()
       (set_loc current_buffer (iferror (rsearch current_buffer 59)
                                        (add 0 0)))
       (set_loc current_buffer (iferror (rsearch current_buffer 59)
                                        (add 0 0)))
       (ift (lp (location current_buffer) 2) just)
       (add_to_loc current_buffer 2)
just
       (store (eq (location current_buffer) 0) pl1_sob))

(defun previous_statement_cmd()
       (previous_statement)
       (next_non_white))

(defun balance_backward(&aux depth start)
       (store 0 depth)
       (start_of_statement)
loop
       (statement_type)
       (ift pl1_eob punt)
       (ift (eq pl1_type 2) scan)
       (previous_statement)
       (ift pl1_sob punt)
       (goto loop)
scan
       (previous_statement)
scan2
       (previous_statement)
       (ift pl1_sob punt)
       (store (location current_buffer) start)
more
       (statement_type)
       (ift pl1_eob punt)
       (ift (eq pl1_type 1) open)
       (ift (eq pl1_type 2) close)
       (ift (gp pl1_type 2) more)
scan3
       (set_loc current_buffer start)
       (goto scan2)
close
       (store (add depth 1) depth)
       (goto scan3)
open
       (ift (lp depth 1) punt2)
       (store (sub depth 1) depth)
       (goto scan3)
punt2
       (previous_statement)
       (start_of_statement)
punt   )


(defun next_statement_cmd()
       (statement_type)
       (next_non_white))


(defun pl1_indent_line(&aux oldpos column newpos char offset)
       (store pl1_start_column column)
indent
       (beginning_of_line)
       (store (location current_buffer) oldpos)
       
;    first hack continuation
       (store (nthr current_buffer -2) char)
       (ift (or (eq char ";")
                (or (eq char 13)
                    (eq char ":"))) nrml)
       (ift (and (eq char "/")
                 (eq (nthr current_buffer -3) "*")) nrml)
       (start_of_statement)
       (store pl1_continuation_column offset)
       (cond ((eq pl1_declaration_hack 0)
              (next_pl1_token)
              (ifnil (or (eq tg1 "dcl") (eq tg1 "declare")) nodcl)
              (ifnil (eq char ",") nodcl)
              (set_loc current_buffer oldpos)
              (previous_line_command)
              (next_non_white)
              (store 0 offset)))
nodcl  (store (sub (add offset
                        (get_hpos current_buffer (location current_buffer))) 1)
              column)
       (set_loc current_buffer oldpos)
       (goto simple)

;    then hack first line
nrml   (previous_statement_cmd)
       (ifnil pl1_sob cool)
       (goto simple)

;    now hack label alone on previous line
cool   (store (location current_buffer) newpos)
       (next_pl1_token)
       (ifnil (eq (nthr current_buffer 0) 58) nolabel)
       (ifnil (eq (nthr current_buffer 1) 13) nolabel)
       (add_to_loc current_buffer 1)
       (next_non_white)
       (goto label)
nolabel
       (set_loc current_buffer newpos)
label
       (store (sub (get_hpos current_buffer (location current_buffer)) 1)
              column)

;    now look back a statement and guess
loop
       (statement_type)
       (ift (lp pl1_type 1) simple)
       (ift (gp pl1_type 2) loop)
       (ift (eq pl1_type 1) open)
       (ift (eq pl1_undent_end 0) simple)
       (store (sub column pl1_indentation) column)
       (goto simple)

open   (store (add column pl1_indentation) column)

;    now delete white space on the line
simple
       (set_loc current_buffer oldpos)
       (set_loc current_buffer          ; is line all white?
                (min (iferror (find_first_not_in_fa current_buffer PLI_White)
                              (length current_buffer))
                     (sub (iferror (search current_buffer 13)
                                   (sub (length current_buffer) 1)) 1)))
       (delete current_buffer (sub oldpos (location current_buffer)))

;    handle undenting end statements
       (ift (eq pl1_undent_end 1) intab)
       (ift (eq (nthr current_buffer 0) 13) intab)
       (statement_type)
       (ifnil (eq pl1_type 2) noind)
       (store (sub column pl1_indentation) column)

;    actually insert tabs and blanks
noind
       (set_loc current_buffer oldpos)
intab
       (ift (lp column 5) inspace)
       (insert 9 current_buffer)
       (store (sub column 5) column)
       (goto intab)
inspace
       (ift (lp column 1) endit)
       (insert 32 current_buffer)
       (store (sub column 1) column)
       (goto inspace)

;    handle iteration if non zero argument
endit
       (store (sub argument 1) argument)
       (ift (lp argument 1) punt)
       (set_loc current_buffer (iferror (search current_buffer 59)
                                        (goto punt)))
       (next_non_white)
       (goto indent)
punt   (store 0 argument))

(defun goto_line_n()
       (set_loc current_buffer 0)
       (store (sub argument 1) argument)
       (next_line_command))

^L