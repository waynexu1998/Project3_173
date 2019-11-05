
(defun APPEND (x y)               ;Append 1.1
  (if (null x)
    y
    (cons (car x) (APPEND (cdr x) y)))
)

(defun REVERSE (x)                ;REVERSE 1.2
  (if (null x)
    nil
    (APPEND (REVERSE (cdr x)) (list (car x)))
  )
)

(defun add3 (x) (+ 3 x))           ;MAP functions 1.3
(defun mult3 (x) (* 3 x))
(defun MAP (x y)                   ;x being the funtion name and y being the list
  (if (null y)
    nil
    (cons (funcall x (car y)) (MAP x (cdr y)))
  )
)

(defun NUB (x)                      ;NUB 1.4
  (if (null x)
    nil
    (if (MEMBER (car x)(cdr x))
      (NUB (cdr x))
      (APPEND (list (car x)) (NUB (cdr x)))
    )
  )
)

(defun MEMBER (x y)                 ;MEMBER 2.1
  (if (null y)
    nil
    (if (equal x (car y))
      t
      (MEMBER x (cdr y))
    )
  )
)

(defun INSERT (x y)                  ;INSERT 2.2
  (if (MEMBER x y)
    y
    (cons x y)
  )
)

(defun INTERSECTION (x y)             ;INTERSECTION 2.3
  (if (null x)
    nil
    (if (MEMBER (car x) y)
      (cons (car x) (INTERSECTION (cdr x) y))
      (INTERSECTION (cdr x) y)
    )
  )
)

(defun UNION (x y)                    ;UNION 2.4
  (NUB
    (if (null y)
      x
      (Append x y)
    )
  )
)

(defun ABS (x)                        ;ABS 3.1
  (if (> x 0)
    x
    (- x)
  )
)

(defun FACTORIAL (x)                  ;FACTORIAL 3.2
  (if (equal x 1)
    1
    (* x (FACTORIAL (- x 1)))
  )
)

(defun RIGHT-TRI (x y z)              ;RIGHT-TRI 3.3
  (if (equal (* z z) (+ (* x x) (* y y)))
    t
    (if (equal (* y y) (+ (* x x) (* z z)))
      t
      (if (equal (* x x) (+ (* z z) (* y y)))
        t
        nil
      )
    )
  )
)

(defun GCD (x y)                       ;GCD 3.4
  (if (equal y 0)
    x
    (GCD y (MOD x y))
  )
)

(defun MOD (x y)                       ;assisting MOD function for 3.4
  (if (< x y)
    x
    (MOD (- x y) y)
  )
)

(defun FACTORS (x y)                    ;listing FACTORS
  (if (< y x)
    (if (equal (MOD x y) 0)
      (cons y (FACTORS x (+ y 1)))
      (FACTORS x (+ y 1))
    )
    '(1)
  )
)

(defun SUM (x)                          ;summing listed factors
  (if (null x)
    0
    (+ (car x) (SUM (cdr x)))
  )
)

(defun PERFECTP (x)                     ;PERFECTP 4.1
  (if (equal x 1)
    nil
    (equal x (SUM (FACTORS x 2)))
  )
)

(defun ABUNDANTP (x)                    ;ABUNDANTP 4.2
  (< x (SUM (FACTORS x 2)))
)

(defun DEFICIENTP (X)                   ;DEFICIENTP 4.3
  (if (equal x 1)
    t
    (> x (SUM (FACTORS x 2)))
  )
)

(defun REPL3 (x y)                      ;repl for 2 arg
  (if (equal y nil)
    nil
    (print-l3 x y)
  )
)

(defun REPL2 (x y)                      ;repl for 2 arg
  (if (equal y nil)
    nil
    (print-l x y)
  )
)

(defun REPL1 (x y)                      ;repl for 1 arg
  (if (equal y nil)
    nil
    (print-l1 x y)
  )
)

(defun print-l (x y)
  (format t "(~a ~a ~a) => ~a~%" x (car y) (car (REVERSE y)) (funcall x (car y) (car (REVERSE y))))
  t
)

(defun print-l1 (x y)
  (format t "(~a ~a) => ~a~%" x (car y) (funcall x (car y)))
  t
)

(defun print-l3 (x y)
  (format t "(~a ~a ~a ~a) => ~a~%" x (car y) (car (cdr y)) (car (REVERSE y)) (funcall x (car y) (car (cdr y)) (car (REVERSE y))))
  t
)

(defun append-out ()                      ;append repl
  (format t "Input a list for APPEND (type nil to quit): ")
    (finish-output nil)
    (if (REPL2 'append (read))
      (append-out)
    )
)

(defun REVERSE-out ()                     ;repl reverse
  (format t "Input a list for REVERSE (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'REVERSE (read))
    (REVERSE-out)
  )
)

(defun MAP-out ()
  (format t "Input a list for MAP (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'MAP (read))
    (MAP-out)
  )
)

(defun NUB-out ()
  (format t "Input a list for NUB (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'NUB (read))
    (NUB-out)
  )
)

(defun MEMBER-out ()
  (format t "Input a list for MEMBER (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'MEMBER (read))
    (MEMBER-out)
  )
)

(defun INSERT-out ()
  (format t "Input a list for INSERT (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'INSERT (read))
    (INSERT-out)
  )
)

(defun INTERSECTION-out ()
  (format t "Input a list for INTERSECTION (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'INTERSECTION (read))
    (INTERSECTION-out)
  )
)

(defun UNION-out ()
  (format t "Input a list for UNION (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'UNION (read))
    (UNION-out)
  )
)

(defun ABS-out ()
  (format t "Input a list for ABS (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'ABS (read))
    (ABS-out)
  )
)

(defun FACTORIAL-out ()
  (format t "Input a list for FACTORIAL (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'FACTORIAL (read))
    (FACTORIAL-out)
  )
)

(defun RIGHTTRI-out ()
  (format t "Input a list for RIGHT-TRI (type nil to quit): ")
  (finish-output nil)
  (if (REPL3 'RIGHT-TRI (read))
    (RIGHTTRI-out)
  )
)

(defun GCD-out ()
  (format t "Input a list for GCD (type nil to quit): ")
  (finish-output nil)
  (if (REPL2 'GCD (read))
    (GCD-out)
  )
)

(defun PERFECTP-out ()
  (format t "Input a list for PERFECTP (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'PERFECTP (read))
    (PERFECTP-out)
  )
)

(defun ABUNDANTP-out ()
  (format t "Input a list for ABUNDANTP (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'ABUNDANTP (read))
    (ABUNDANTP-out)
  )
)

(defun DEFICIENTP-out ()
  (format t "Input a list for DEFICIENTP (type nil to quit): ")
  (finish-output nil)
  (if (REPL1 'DEFICIENTP (read))
    (DEFICIENTP-out)
  )
)

(defun main ()                    ;main function
  (princ "Welcome to Project3")
  (terpri)
  (princ "*** List Functions")
  (terpri)
  (princ "(APPEND (1 3 X A) (4 2 B)) => (1 3 X A 4 2 B)")
  (terpri)
  (princ "(REVERSE (A B C D)) => (D C B A)")
  (terpri)
  (princ "(MAP ADD3 (1 2 3 4)) => (4 5 6 7)")
  (terpri)
  (princ "(NUB (1 1 2 4 1 2 5)) => (1 2 4 5)")
  (terpri)
  (terpri)
  (princ "*** Set Functions")
  (terpri)
  (princ "(MEMBER A (B C A D)) => T")
  (terpri)
  (princ "(INSERT A (B C D)) => (A B C D)")
  (terpri)
  (princ "(INTERSECTION (A B C) (A C D)) => (A C)")
  (terpri)
  (princ "(UNION (A B C) (A C D)) => (A B C D)")
  (terpri)
  (terpri)
  (princ "*** Math Functions")
  (terpri)
  (princ "(ABS -7) => 7")
  (terpri)
  (princ "(FACTORIAL 5) => 120")
  (terpri)
  (princ "(RIGHT-TRI 3 4 5) => T")
  (terpri)
  (princ "(GCD 8 12) => 4")
  (terpri)
  (terpri)
  (princ "*** Required Functions")
  (terpri)
  (princ "(PERFECTP 6) => T")
  (terpri)
  (princ "(ABUNDANTP 12) => T")
  (terpri)
  (princ "(DEFICIENTP 5) => T")
  (terpri)
  (terpri)




  (princ "(APPEND (1 3 X A) (4 2 B)) => (1 3 X A 4 2 B)" )
  (terpri)

  (APPEND-out)
  (princ "(REVERSE (A B C D)) => (D C B A)")
  (terpri)

  (REVERSE-out)
  (princ "(MAP ADD3 (1 2 3 4)) => (4 5 6 7)")
  (terpri)

  (MAP-out)
  (princ "(NUB (1 1 2 4 1 2 5)) => (1 2 4 5)")
  (terpri)

  (NUB-out)
  (princ "(MEMBER A (B C A D)) => T")
  (terpri)

  (MEMBER-out)
  (princ "(INSERT A (B C D)) => (A B C D)~%(INSERT A (A B C D)) => (A B C D)")
  (terpri)

  (INSERT-out)
  (princ "(INTERSECTION (A B C) (A C D)) => (A C)")
  (terpri)

  (INTERSECTION-out)
  (princ "(UNION (A B C) (A C D)) => (A B C D)")
  (terpri)

  (UNION-out)
  (princ "(ABS 7) => 7~%(ABS -7) => 7")
  (terpri)

  (ABS-out)
  (princ "(FACTORIAL 5) => 120")
  (terpri)

  (FACTORIAL-out)
  (princ "(RIGHT-TRI 3 4 5) => T~%(RIGHT-TRI 1 2 3) => NIL")
  (terpri)

  (RIGHTTRI-out)
  (princ "(GCD 8 12) => 4")
  (terpri)

  (GCD-out)
  (princ "(PERFECTP 5) => NIL~%(PERFECTP 6) => T")
  (terpri)

  (PERFECTP-out)
  (princ "(ABUNDANTP 5) => NIL~%(ABUNDANTP 12) => T")
  (terpri)

  (ABUNDANTP-out)
  (princ "(DEFICIENTP 5) => T~%(DEFICIENTP 12) => NIL")
  (terpri)

  (DEFICIENTP-out)
)

(main)
