
(setq trace ())
(setq debug nil)
(setq list ())
(setq pushp nil)
(setq new_cons nil)
(setq debug_asm nil)
(declare 'create_seg "lisp_create$lisp_create" '(y))
(declare 'com_arg "lisp_create$com_arg" ())

(defun macro sinemac (body)
     (list 'progn (list 'putprop  (list 'quote (cadr body))
                        (list 'quote (setq real_fun_name (gensym)))
                        ''macro)
                  (cons 'defun (cons real_fun_name (cddr body)))))

(defun top_level ()
     (cond (debug_asm (setq debug_asm
                      (prog () loop (print (eval (progn (terpri) (read))))
                                    (go loop)))))
     (setq debug_asm t)       ;if it bombs out in the middle
     (setq output_filename
           (catenate (setq name (com_arg)) ".sine"))
     (asm (catenate name ".asm"))
     (setq debug_asm ())
     (quit))

(defun print_out (foo)
     (do ((i 1 (1+ i)) (len (string_length foo)))
         ((greaterp i len))
          (tyo (get_ascii foo i))))


(defun asm (filename &aux function_list pc vbls statics vbl_list
                          documentation temps)
     (or new_cons (progn (append_cons_seg) (setq new_cons t)))
     (gc)

;;; Read in the file and make all the variables and function names known

     (setq statics 1)              ;Make room for the vbl table header
     (setq vbls 1)                 ;make room for documentation
     (cond ((eq (setq filei (open (print filename))) 0)
            (print "File not found") (setq debug_asm ()) (quit)))

     (setq pushing (get_opcode '+push))
     (setq storing (remainder (get_opcode 'store) \1000))
     (setq asing (remainder (get_opcode 'as) \1000))
     (setq consing (remainder (get_opcode 'cons) \1000))
     (setq rplacaing (remainder (get_opcode 'rplaca) \1000))
     (setq rplacding (remainder (get_opcode 'rplacd) \1000))
     (setq filling_vbl_array (remainder (get_opcode 'fill_vbl_array) \1000))
     (setq documentation '!#$%&)

     (do ((line (read filei) (read filei)) (op))
         ((eq line '**eof**) ())
;         (set_gc_flag 1)
          (cond ((eq (setq op (car line)) 'variable)
                 (mapcar '(lambda (x) (add_symbol x 'variable))
                         (cdr line)))
                ((eq op 'static)
                 (mapcar '(lambda (x) (add_symbol x 'static))
                         (cdr line)))
                ((eq op 'defun)
                 (setq function_list (cons line function_list))
                 (add_symbol (cadr line) 'variable))
                ((eq op 'documentation)
                 (cond ((stringp (cadr line))
                        (putprop (setq documentation (gensym)) (cadr line)
                                 'string))
                       (t (rplaca line 'defun)
                          (rplacd line (cons '!#$%& (cdr line))))))
                (t (print_warning "illegal top level op ignored" op))))
     (close filei)

;;; And grovel over everything and output it.

     (create_seg output_filename)
     (cond ((eq (setq fileo (open (print output_filename))) 0)
            (print "File not found") (setq debug_asm ()) (quit)))

     (setq object (make_array_header 16 (addr fileo) 32000))
     (setq pc 1)    ;leave room for ptr to entry map

;    (set_gc_flag 1)
;    (mapcar '(lambda (x) (print (plist x))) vbl_list)

     (mapcar '(lambda (x) (emit_fun (parse_fun x pc)))
          (nreverse function_list))

;;; Now the entry map

     (terpri)
     (print (list "max_code:" pc))

     (as (* pc 2) object 0)             ;offset of entry map
     (emithw (+ vbls statics -1))       ;output the vbl count

     (do ((vbl (setq vbl_list (cons documentation (nreverse vbl_list)))
               (cdr vbl))
          (thing) (str))
         ((null vbl) ())
          (cond ((setq str (get (setq thing (car vbl)) 'string))
                 (emit_name str -1))
                (t (emit_name thing (or (get thing 'pc) 0)))))

     (emithw 0)     ;now a terminator
     (emithw 0)
     (mapcar '(lambda (x) (setplist x '(a b))) vbl_list)
     (print (list 'max_pc: pc))
     (close fileo))

(defun parse_fun (function pc
          &aux fun args code initial_pc num_args)
       (setq string_list ())
     (and list (progn (terpri) (terpri) (print (cadr function))))
     (setq initial_pc pc)
     (setq temps 9)      ; this is tied to the size of the stack frame
     (cond ((atom function) (print_warning "bad fun def" function))
           ((lessp (length function) 3)
               (print_warning "fun def too short" function))
           (t (putprop (cadr function) pc 'pc)    ;for entry map
              (setq pc (1+ pc))                   ;room for num_args
              (setq args (caddr function))
              (setq num_args (do ((i 0 (1+ i)) (arg args (cdr arg)))
                                 ((or (null arg)(eq (car arg) '&aux)) i)))
              (do ((i (- 0 num_args) (1+ i)) (arg args (cdr arg)) (arg_name))
                  ((null arg) ())
                    (setq arg_name (car arg))
                    (or (eq arg_name '&aux)
                        (cond ((memq arg_name vbl_list)
                                   (generate (list 'bind arg_name))
                                   (and (minusp i)
                                        (generate (list 'store (list 'arg i)
                                                        arg_name))))
                               (t (cond ((minusp i)(add_symbol arg_name 'arg i))
                                        (t (add_symbol arg_name 'temp)))))))
              (setq fun (cdddr function))
              (prog (line op)
loop           (and list (progn (terpri) (print (list "source     " (car fun)))))
               ((lambda (pushp) (parse_call (car fun))) (null (cdr fun)))
               (and (setq fun (cdr fun))
                    (go loop))
               (generate '(return))
               (mapcar '(lambda (x) (parse_call x)
                                    (parse_call (list 'string (symeval x))))
                       string_list)
               (print (list (cadr function) (- pc initial_pc)))
               (return (nreverse code))))))

(defun parse_call (call &aux nargs arg_count call_code opcode this_arg fun_name
                             real_fun_name)
     (and trace (print (list pushp call)))
     (cond ((numberp call) (parse_call (list 'push call)))
           ((atom call) (report) (add_symbol call 'label))
           ((setq real_fun_name (get (setq op (car call)) 'macro))
            (apply real_fun_name (cdr call)))
           ((eq op 'temp) (report) (add_symbol (cadr call) 'temp))
           ((eq op 'string) (generate call
                                      (1+ (/ (1+ (string_length
                                                  (cadr call))) 2))))
           (t (cond ((setq opcode (get_opcode (setq fun_name (car call))))
                     (setq arg_count 0)
                     (setq nargs 0)

;;; Now just count the number of args that will be on the stack since
;;; we have to reference them in reverse order.

                     (do ((call_args (cdr call) (cdr call_args)))
                         ((null call_args) ())
                          (setq nargs (1+ nargs))
                          (setq this_arg (car call_args))
                          (or (atom this_arg)
                              (progn ((lambda (pushp) (parse_call this_arg)) t)
                                     (setq arg_count (1+ arg_count)))) )

                     (or (null debug)                  ;don't check
                         (eq nargs (cddr (assq fun_name opcode_table)))
                         (error "bad nargs" call))

;;; Build up the instruction preparation code.

                     (do ((call_args (cdr call) (cdr call_args))
                          (argno 0)
                          (stratom))
                         ((null call_args) ())
                         (setq this_arg (car call_args))
                         (cond ((stringp this_arg)
                                (setq call_code
                                      (cons (get_string_object this_arg
                                                               opcode)
                                            call_code)))
                               ((atom this_arg)
                                (setq call_code (cons this_arg call_code)))
                               (t (setq call_code
                                        (cons (list 'sp_rel
                                                    (- (setq argno (1+ argno))
                                                    arg_count 1))
                                              call_code))) ))
                     (setq to_generate (cons (cond (debug fun_name) (t opcode))
                                             (nreverse call_code)))
                     (and list
                          (print (list pc (cons fun_name (cdr to_generate)))))
                     (generate to_generate))

;;;  In this case it is probably a function not an instruction

                    ((or (eq (get fun_name 'type) 'variable)
                         (progn (print_warning "Assuming external function"
                                               fun_name)
                                (add_symbol fun_name 'variable)
                                t)) 
                     (do ((call_args (cdr call) (cdr call_args)))
                         ((null call_args) ())
                         (setq this_arg (car call_args))
                         (cond ((atom this_arg)   ;always push
                                (parse_call (list '+push this_arg)))
                               (t ((lambda (pushp) (parse_call this_arg)) t))))
                     (parse_call (list 'call (car call) (length (cdr call)))))
                    (t (error "undefined function" fun_name)) ))))

(defun get_string_object (string opcode &aux string_label)
       (cond

;;;  Here we convert single character long strings into inline integers
;;;  for efficiency.
        ((eq (string_length string) 1) (get_ascii string 1))

;;;  If the opcodes is a push then it will be on the stack and therefore
;;;  must be global.  So do the real stuff.
        ((or (eq opcode pushing)
             (eq (remainder opcode \1000) storing)
             (eq (remainder opcode \1000) asing)
             (eq (remainder opcode \1000) consing)
             (eq (remainder opcode \1000) rplacaing)
             (eq (remainder opcode \1000) rplacding)
             (eq (remainder opcode \1000) filling_vbl_array))
         (putprop (setq stratom (gensym)) string 'string)
         (add_symbol stratom 'variable)
         stratom)

;;;  Here it is only a operand to an instruction.  So a pc relative type
;;;  string is good enough.
        (t
         (setq string_list (cons (setq string_label (gensym)) string_list))
         (set string_label string)           ;remember the string
         string_label)))

(defun report ()
     (and list (print (list pc call))))

(defun add_symbol (sym type &opt value)
;    (or (get sym 'type) (error "multiple definition" sym))
     
     (putprop sym type 'type)
     (putprop sym
              (cond ((eq type 'arg) value)
                    ((eq type 'static) statics)
                    ((eq type 'variable) vbls)
                    ((eq type 'label) pc)
                    ((eq type 'temp) temps)
                    (t value))
             'value)
;;; We also need to keep track of the temps and the variables separately

     (cond ((eq type 'temp) (setq temps (1+ temps)))
           ((eq type 'static) (setq statics (1+ statics)))
           ((eq type 'variable) (setq vbls (1+ vbls))
                                (setq vbl_list (cons sym vbl_list))) ))

(defun generate (data &opt new_pc)
     (and debug (print data))
     (setq code (cons (cond ((numberp (car data)) data)
                            (t (cons (get_opcode (car data)) (cdr data))))
                      code))
     (setq pc (+ pc (or new_pc (length data)))) )

(defun emit_name (name type &aux name_string)
     (setq name_string (or (and (stringp name) name)
                           (get_pname name)))
     (emithw type)
     (emithw (string_length name_string))
     (emitchars name_string))

(defun emit_fun (code)

;;; First do the entry sequence

     (emithw (- temps 9))   ; because of the stack offset

     (do ((inst code (cdr inst)) (line) (op))
         ((null inst) ())
          (setq line (car inst))
          (setq op (car line))
          (cond ((numberp op) (emithw op))
;               ((eq op 'number) (setq op (eval_operand (cadr line)))
;                                (emithw (cond ((minusp op) -1) (t 0)))
;                                (emithw op)
;                                (go next))
                ((eq op 'string) (emithw (string_length (cadr line)))
                                 (emitchars (cadr line)) (go next))
                (t (emithw (or (get_opcode op) (error "ii" op)))))
          (do ((foo (cdr line) (cdr foo)))
              ((null foo) ())
               (emithw (eval_operand (car foo))))
next      ))

(defun get_opcode (op &aux string opcode)
     (cond
          ((eq op 'string) op)
;         ((eq op 'number) op)
          ((eq (get_ascii (setq string (get_pname op)) 1) (CtoI "+"))
           (cadr (assq (intern (setq string (substr string 2)))
                               opcode_table)))    ;force a push
          ((setq opcode (cadr (assq op opcode_table)))
           (cond (pushp opcode)
                 (t (logior opcode \1000))   ;inhibit the push
               ))))

(defun eval_operand (operand)
     (cond ((numberp operand) (logand \7fff operand))
           ((atom operand)
               (prog (sym_ent)
try_again           (or (setq sym_ent (get operand 'value))
                        (progn (print_warning "Assuming external variable"
                                              operand)
                               (add_symbol operand 'variable)
                               (go try_again)))
                    (go (get operand 'type))
constant            (return (logand \7fff sym_ent))
variable            (return (logior \e000 (logand \1fff (+ statics
                                                           sym_ent))))     ;11
static              (return (logior \e000 (logand \1fff sym_ent)))    ;11
arg       temp      (return (logior \a000 (logand \1fff sym_ent)))    ;01
label               (return (logior \8000                             ;00
                                    (logand \1fff
                                            (- sym_ent (1+ pc)))))))
          ((eq (car operand) 'sp_rel)                                 ;10
               (logior \c000 (logand \1fff (cadr operand))))
          ((eq (car operand) 'arg)                                    ;01
               (logior \a000 (logand \1fff (cadr operand))))
          (t (error "bad code!" operand))))

(defun emitchars (string)
     (do ((i 1 (+ i 2)) (max (string_length string)))
         ((> i max) ())
          (emithw (plus (* 256 (get_ascii string i))
                        (or (get_ascii string (1+ i)) 0)))))

(defun emithw (hw)
     (as hw object pc)
     (setq pc (1+ pc)))

(defun error ( &rest msg )
     (terpri)
     (print (cons 'ERROR msg))
     #&%@adad)

(defun print_warning ( &rest msg )
     (terpri)
     (print (cons 'WARNING msg)))

^L