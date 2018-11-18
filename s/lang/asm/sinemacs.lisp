
(sinemac errset ( &quote string form handler &aux label1 label2 label3)
     (parse_call (list 'handle string (setq label2 (gensym))))
     (mapcar 'parse_call handler)
     (parse_call (list 'goto (setq label3 (gensym))))
     (parse_call label2)           ;define label for start of code
     (mapcar 'parse_call form)
     (parse_call '(revert))
     (parse_call label3))

(sinemac list ( &quote &rest args )
         (parse_call (recursive_list args)))

(defun recursive_list (a)
       (cond ((eq (length a) 1) (list 'cons (car a) '(nil)))
             (t (list 'cons (car a) (recursive_list (cdr a))))))

(sinemac caddr (&quote a)
         (parse_call (list 'car (list 'cddr a))))

(sinemac cdddr (&quote a)
         (parse_call (list 'cdr (list 'cddr a))))

(sinemac cadddr (&quote a)
         (parse_call (list 'cadr (list 'cddr a))))

(sinemac cddddr (&quote a)
         (parse_call (list 'cddr (list 'cddr a))))

(sinemac return_value (&quote value)
         (parse_call (list '+push value))
         (parse_call '(return)))

(sinemac iferror ( &quote body &rest error &aux label1 )
         (parse_call body)
         (parse_call (list 'bnoerr (setq label1 (gensym))))
;throw away whatever returned an error
         (cond (pushp (parse_call '(discard))))
;generate the code for what to do if error..
         (mapcar 'parse_call error)
         (parse_call label1))

(sinemac cond ( &quote &rest body &aux end_atom next not_first )
     (setq end_atom (gensym))
     (mapcar '(lambda (x) (and not_first (progn
                                    (parse_call (list 'goto end_atom))
                                    (parse_call next)))
                          (setq not_first t)
                          (parse_call (list 'ifnil (car x)  ;get the predicate
                                                   (setq next (gensym))))
                          (parse_call (cons 'progn (cdr x))))
             body)
     (parse_call next)
     (parse_call end_atom))   ;define termination point

(sinemac progn ( &quote &rest body )
     (do ((item body (cdr item)))
         ((null (cdr item)) (parse_call (car item)))        ;let it push
          ((lambda (pushp) (parse_call (car item))) nil)))  ;no pushes

(sinemac for ( &quote index init max &rest body &aux loop end pushp)
     (setq pushp nil)                        ;for is for side effect only
     (parse_call (list 'store init index))
     (parse_call (setq loop (gensym)))
     (parse_call (list 'ift (list 'gp index max) (setq end (gensym))))
     (parse_call (cons 'progn body))
     (parse_call (list 'store (list 'add index 1) index))
     (parse_call (list 'goto loop))
     (parse_call end))

(sinemac cat_into ( &quote gnirt &rest others)
         (parse_call (list 'delete gnirt 99999))
         (mapcar '(lambda (x) (parse_call (list 'insert x gnirt)))
                 others))

(sinemac bind_key ( &quote key function)
     (parse_call (list 'bind_array_cell (car (decode key)) (cdr (decode key))
                       (list 'quote function))))

(defun macro store (x)
     (rplaca x 'setq)
     (rplacd x (list (caddr x) (cadr x)))
     x)

(defun decode (key &aux string n char next ortho
                               control_x control meta index modifier)
     (cond ((symbolp key) (setq string (get_pname key)))
           (t (setq string key)))
(prog ()
     (store 1 n)
     (store (store (store (store (store 0 control_x) index) control) meta) ortho)
parse_loop
     (store (upper_case (store (get_ascii string n) char)) modifier)
     (store (get_ascii string (+ n 1)) next)
     (cond  ((null char) (go got_it))
            (t (cond ((and (eq (upper_case index) \58) (eq control 1))
                        (store 1 control_x) (store (store 0 control) index)))))
     (cond ((null next) (or (eq index 0) (go error))
                         (store char index) (go got_it))
           ((and (eq modifier \43) (eq next \2d)) (or (eq control 0) (go error))
                                                  (store 1 control))
           ((and (eq modifier \4d) (eq next \2d)) (or (eq meta 0) (go error))
                                                  (store 1 meta))
           ((and (eq modifier \4f) (eq next \2d)) (or (eq ortho 0) (go error))
                                                  (store 1 ortho))
           ((and (eq modifier \44)
                 (and (eq (upper_case next) \45)
                      (eq (upper_case (get_ascii string (+ n 2))) \4c)))
            (store \7f index) (go got_it))
           ((eq modifier \5e) (or (eq index 0) (go error))
                              (store 1 control) (store next index))
           (t (store char index) (store (+ n 1) n) (go parse_loop)))
     (store (+ 2 n) n)
     (go parse_loop)
error
     (return (cons 0 0))
got_it
     (cond ((eq index 0) (go error)))
     (cond ((eq control 1) (store (logand index \1f) index)))
     (return (cond ((eq control_x 1)
                    (and (or (eq ortho 1)(eq meta 1)) (go error))
                    (cons 'C-X_dispatch index))
                   ((eq meta 1) (cons 'M_dispatch index))
                   ((eq ortho 1) (cons 'O_dispatch index))
                   (t (cons 'dispatch index))
                   (t (cons 0 0))))))

(defun upper_case (foo)
       (cond ((null foo) foo)
             ((and (> foo \60) (< foo \7b)) (- foo \20))
             (t foo)))

(sinemac do_while (&quote test &rest body &aux loop end)
         (parse_call (setq loop (gensym)))
         (parse_call (list 'ifnil test (setq end (gensym))))
         (mapcar 'parse_call body)
         (parse_call (list 'goto loop))
         (parse_call end))

(sinemac do_until (&quote test &rest body &aux loop end)
         (parse_call (setq loop (gensym)))
         (parse_call (list 'ift test (setq end (gensym))))
         (mapcar 'parse_call body)
         (parse_call (list 'goto loop))
         (parse_call end))
wotT (5.572 0 3.21) >s>lang>asm

