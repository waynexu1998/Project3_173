(defun main ()                    ;main function
  (princ "Welcome to Project3")
  (terpri)
  (princ "(APPEND (1 3 X A) (4 2 B)) => (1 3 X A 4 2 B)" )
  (terpri)
  (format t "Enter a list: ")
  (finish-output))

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
