
(documentation ">doc>sm>abbrev_mode.doc")

(variable dispatch M_dispatch C-X_dispatch keep_on_tunneling token_hackers)
(variable tg1 alphanumerics)
(variable read_line replace blow_out)
(variable current_buffer echo_buf tg char)

(variable abbrev_table)
(variable old_space old_cr)

(defun abbrev_mode ( &aux old_space old_cr )
       (store (cons 'expand_last_token token_hackers) token_hackers)
       (keep_on_tunneling))

(defun user_init ()
       (store (make_buffer) abbrev_table)
       (store (make_gnirt) tg1))

(defun load () (load_abbrevs))
(defun load_abbrevs ( &aux file )
       (store (read_line "Load Abbrevs From File: " 13) file)
          ;if [filename]=""
          ;    then get >u>ss>[userid].abbrevs
          ;    else if its in [filename]
          ;              then use it
          ;              else see if its in >u>ss>[userid].[filename]
       (cond ((eq (length file) 0)
                  (cond ((gep (read_file echo_buf (cat_into tg
                         ">u>ss>" (call_af "user id") ".abbrevs")) 0) (t))
                        ((t) (blow_out "File Not Found"))))
             ((gep (read_file echo_buf file) 0) (replace tg file))
             ((gep (read_file echo_buf (cat_into tg
                         ">u>ss>" (call_af "user id") "." file)) 0) (t))
             ((t) (blow_out "File Not Found")))

       (set_loc abbrev_table 0)
       (insert echo_buf abbrev_table)
       (set_loc echo_buf 0)
       (delete echo_buf (length echo_buf))
       (insert tg echo_buf))

(defun def () (define_abbrev))
(defun define_abbrev ( &aux abbrev expansion )
       (store (read_line "Abbrev: " 32) abbrev)
       (cat_into tg "Expansion for '" abbrev "': ")
       (replace tg1 abbrev)
retry  (ift (eq (length (store (read_line tg 13) expansion)) 0) retry)
          ;put abbrev and expansion into abbrev table "^^abbrev^_expansion"
       (set_loc abbrev_table 0)
       (insert \1e abbrev_table)
       (insert tg1 abbrev_table)
       (insert \1f abbrev_table)
       (insert expansion abbrev_table)
       (insert 13 abbrev_table))

(defun save () (save_abbrevs))
(defun save_abbrevs ( &aux file)
       (store (make_buffer) file)
       (insert (read_line "Save Abbrevs Into File: " 13) file)
       (set_loc file 0)
          ;if [filename]=""
          ;    then use >u>ss>[userid].abbrevs
          ;    else if substr([filename],1,6)=">u>ss>"
          ;              then use >u>ss>[userid].[rest of name]
          ;              else use [filename]
       (cond ((eq (length file) 0) 
                    (insert ">u>ss>" file)
                    (insert (call_af "user id") file)
                    (insert ".abbrevs" file))
             ((looking_atp ">u>ss>" file 0)
                    (set_loc file 6)
                    (insert (call_af "user id") file)
                    (insert "." file)))
       (write_file abbrev_table file)
       (set_loc echo_buf 0)
       (delete echo_buf (length echo_buf))
       (insert file echo_buf))

(defun expand_last_token ( &aux found )
       (replace tg \1e)
       (insert_region current_buffer (iferror
                                      (find_first_not_in_ba current_buffer
                                                          alphanumerics)
                                      (push 0))
                      (location current_buffer)
                      tg)
       (insert \1f tg)
       (set_loc abbrev_table 0)
       (set_loc abbrev_table
           (store (iferror (search abbrev_table tg) (goto regular)) found))
       (delete current_buffer (sub 2 (length tg)))
       (insert_region abbrev_table found (sub (search abbrev_table 13) 1)
                      current_buffer)
regular)
^L