#lang racket

;; Laindon Burnett 20656393
;; Imran Saleh 20550301

;; stack pointer
(define sp 1)
;; label-counter
(define lc 0)

;; adds _ to the front of a variable symbol to indicate
;;    that it comes from the program
;; sym -> sym
(define (prog-var sym)
  (define l (string->list (symbol->string sym)))
  (string->symbol (list->string (cons #\_ l))))

;; generates temp variable name
(define (gen-label n)
  (define c (string->list (number->string n)))
  (string->symbol (list->string (cons #\L c))))

;; creates new temp variable
(define (new-label)
  (define nl (gen-label lc))
  (set! lc (add1 lc)) nl)

;; checks if an expression is simple or whether recursion is needed
(define (simple? exp)
  (or (number? exp) (symbol? exp) (boolean? exp)))

(define (aexp sym)
  (match sym
    ['+ 'add]['* 'mul]['- 'sub]['div 'div]['mod 'mod]
    [else (error "invalid aexp operator")]))

(define (aexp-gen exp)
  (cond [(simple? exp) empty]
        [else
         (define op (aexp (first exp)))
         (define opd1 (second exp))
         (define opd2 (third exp))
         (define s1 (simple? opd1))
         (define s2 (simple? opd2))
         (cond [(not (or s1 s2))
                (define c (aexp-gen opd1)) (define val-loc (sub1 sp))
                (append c (append (aexp-gen s2)
                                  `((,op (-1 SP) (,val-loc SPL) (-1 SP)))))]
               [(not s1) (define c (aexp-gen opd1))
                   (when (symbol? opd2) (set! opd2 (prog-var opd2)))
                   (cond [(> sp 25) (error "stack overflow")]
                         [else (append c `((,op (-1 SP) (-1 SP) ,opd2)))])]
               [(not s2) (define c (aexp-gen opd2))
                   (when (symbol? opd1) (set! opd1 (prog-var opd1)))
                   (cond [(> sp 25) (error "stack overflow")]
                         (else (append c `((,op (-1 SP) ,opd1 (-1 SP))))))]
               [else (set! sp (add1 sp))
                     (when (symbol? opd2) (set! opd2 (prog-var opd2)))
                     (when (symbol? opd1) (set! opd1 (prog-var opd1)))
                     `((,op (0 SP) ,opd1 ,opd2) (add SP SP 1))])]))

(define (bexp sym)
  (match sym
    ['= 'equal]['> 'gt]['>= 'ge]['< 'lt]['<= 'le]
    ['not 'lnot]['and 'land] ['or 'lor]
    [else (error "invalid bexp comparator operator")]))

(define (bexp-gen exp)
  (if (simple? exp) empty
    (match exp
      [`(and ,x) (bexp-gen x)]
      [`(or ,x) (bexp-gen x)]
      [`(and ,x ...) (and/or-case exp)]
      [`(or ,x ...) (and/or-case exp)]
      [`(not ,x) (notcase exp)]
      [`(,f ,x ,y) (comparecase exp)])))

(define (notcase exp)
  (define opd1 (second exp))
  (if (simple? opd1) `((lnot (0 SP) ,opd1) (add SP SP 1))
      (append (bexp-gen opd1) `((lnot (-1 SP) (-1 SP))))))

(define (and/or-case raw)
  (define exp (and/or-make raw))
  (define op (bexp (first exp)))
  (define opd1 (second exp))
  (define opd2 (third exp))
  (define s1 (simple? opd1))
  (define s2 (simple? opd2))
  (cond [(not (or s1 s2))
         (define c (bexp-gen opd1)) (define val-loc (sub1 sp))
         (append c (append (bexp-gen opd2)
                           `((,op (-1 SP) (,val-loc SPL) (-1 SP)))))]
        [(not s1) (define c (bexp-gen opd1))
            (cond [(> sp 25) (error "stack overflow")]
                  [else (append c `((,op (-1 SP) (-1 SP) ,opd2)))])]
        [(not s2) (define c (bexp-gen opd2))
            (cond [(> sp 25) (error "stack overflow")]
                  (else (append c `((,op (-1 SP) (-1 SP) ,opd1)))))]
        [else (set! sp (add1 sp))
              `((,op (0 SP) ,opd1 ,opd2) (add SP SP 1))]))

(define (comparecase exp)
  (define op (bexp (first exp)))
  (define opd1 (second exp))
  (define opd2 (third exp))
  (define s1 (simple? opd1))
  (define s2 (simple? opd2))
  (cond [(not (or s1 s2))
         (define c (aexp-gen opd1)) (define val-loc (sub1 sp))
         (append c (append (aexp-gen s2)
                            `((,op (-1 SP) (,val-loc SPL) (-1 SP)))))]
        [(not s1) (define c (aexp-gen opd1))
            (when (symbol? opd2) (set! opd2 (prog-var opd2)))
            (cond [(> sp 25) (error "stack overflow")]
                  [else (append c `((,op (-1 SP) (-1 SP) ,opd2)))])]
        [(not s2) (define c (aexp-gen opd2))
            (when (symbol? opd1) (set! opd1 (prog-var opd1)))
            (cond [(> sp 25) (error "stack overflow")]
                  (else (append c `((,op (-1 SP) ,opd1 (-1 SP))))))]
        [else (set! sp (add1 sp))
              (when (symbol? opd1) (set! opd1 (prog-var opd1)))
              (when (symbol? opd2) (set! opd2 (prog-var opd2)))
              `((,op (0 SP) ,opd1 ,opd2) (add SP SP 1))]))

(define (and/or-make exp)
  (cond [(= (length exp) 3) exp]
        [else (and/or-make
               (cons (first exp)
                     (cons (take exp 3) (drop exp 3))))]))

;; generates code for print stmt
;; `(print ,exp) -> (listof (a-primp stmts))
(define (print-gen stmt)
  (define exp (second stmt))
  (cond [(string? exp) `((print-string ,exp))]
        [(number? exp) `((print-val ,exp))]
        [(symbol? exp) `((print-val ,(prog-var exp)))]
        [else (define c (aexp-gen exp)) (set! sp (sub1 sp))
              (append c '((sub SP SP 1) (print-val (0 SP))))]))

;; generates code for while loop
;; `(while ,exp ,stmt1 ...) -> (listof a-primp stmt)
(define (while-gen stmt)
  (define exp (bexp-gen (second stmt))) ;; exp-code
  (define loop-top (new-label)) ;; label for top of loop
  (define bdy (new-label)) ;; label for body
  (define loop-end (new-label)) ;; label for end of loop
  (define cnd (cons `(label ,loop-top) exp))
  (set! sp (sub1 sp))
  (define ctrl `((sub SP SP 1) (branch (0 SP) ,bdy)
                     (jump ,loop-end) (label ,bdy)))
  (define stmts-code (make-stmt (rest (rest stmt))))
  (define ctrl2 (cons `(jump ,loop-top) `((label ,loop-end))))
  (append cnd (append ctrl (append stmts-code ctrl2))))

;; makes the list of program variables into a list of data
;; (listof (sym val)) -> (listof ('data sym val))
(define (make-data lov)
  (cond [(empty? lov) empty]
        [else (cons `(data ,(prog-var (first (first lov)))
                           ,(second (first lov)))
                    (make-data (rest lov)))]))

;; translates the actual program into nice things
(define (make-stmt p)
  (cond [(empty? p) empty]
        [else
          (match (first p)
           [`(print ,a) (append (print-gen (first p))
                                (make-stmt (rest p)))]
           [(cons 'seq a) (append (make-stmt a)
                                  (make-stmt (rest p)))]
           [`(while ,a ...) (append (while-gen (first p))
                                    (make-stmt (rest p)))]
           [`(iif ,a ,b ,c) (append (iif-gen (first p))
                              (make-stmt (rest p)))]
           [`(set ,a ,exp) (append (set-gen (first p))
                              (make-stmt (rest p)))]
           [`(skip) '()])]))

;; main function
(define (compile-simp s-exp)
  (match s-exp
    ;; lov is list of variables, and p is the rest of the program
    [(cons 'vars (cons lov p))
     (cons `(add SP SP 1)
           (append (append (make-stmt p) (cons '(halt)
                                               (make-data lov)))
             `((data SPL SP)(data SP SP) (data (100 0)))))]))

;; generates the code for an iif statement
(define (iif-gen stmt)
  (define bexp (bexp-gen (second stmt)))
  (define texp (new-label))
  (define fexp (new-label))
  (define endif (new-label))
  (define conditional (append bexp (append `((sub SP SP 1) (branch (0 SP) , texp))
                            (cons `(jump, fexp) `((label ,texp))))))
  (define stmt-one (make-stmt `(,(third stmt))))
  (define between (cons `(jump ,endif) `((label ,fexp))))
  (define stmt-two (make-stmt `(,(fourth stmt))))
  (append conditional (append stmt-one (append between (append stmt-two `((label ,endif)))))))

;; creates a new list of variables based on the specified parameters
(define (set-gen stmt)
  (define exp (third stmt))
  (cond [(simple? exp)
         (when (symbol? exp) (set! exp (prog-var exp)))
         `((move ,(prog-var (second stmt)) ,exp))]
        [else (set! sp (sub1 sp))
              (append (aexp-gen exp)
                      `((sub SP SP 1)
                        (move ,(prog-var (second stmt)) (0 SP))))]))

;(define empty-prog '(vars [(x 3)] (print (+ x 4))))
;(compile-simp empty-prog)

;(define iif-prog '(vars [(x 3) (y 4)]
;                        (iif (or (and (< x y) (> y x))
;                                  (and (= x y) false))
;                             (print "aeou") (print (div 3 4)))))
;(compile-simp iif-prog)

;(define prog '(vars [(x 10) (y 1)]
;  (while (> x 0)
;     (set y (* 2 y))
;     (set x (- x 1))
;     (print y)
 ;    (print "\n"))))
;(compile-simp prog)

;(define p3 '(vars [(i 1) (j 0) (acc 0)]
;  (while (<= i 10000)
 ;    (set j 1)
  ;   (set acc 0)
;     (while (< j i)
;        (iif (= (mod i j) 0)
;             (set acc (+ acc j))
;             (skip))
;        (set j (+ j 1)))
;     (iif (= acc i)
;          (seq
 ;;           (print i)
 ;           (print "\n"))
 ;         (skip))
 ;    (set i (+ i 1)))))
;(compile-simp p3)

;(define q '(vars [(a 0) (b 2)] (iif (> a b) (skip) (print "aou"))))
;(compile-simp q)

;(define p5 '(vars [(n 10) (fj 1) (fjm1 0) (ans 0)]
;   (iif (= n 0)
;        (set ans fjm1)
;        (seq
;         (while (> n 1)
;           (set fj (+ fj fjm1))
;            (set fjm1 fj)
;            (set n (- n 1)))
;         (set ans fj)))
;   (print ans)))
;(compile-simp p5)

;(define p6 '(vars [(n 10) (fj 1) (fjm1 0) (t 0) (ans 0)]
;   (iif (= n 0)
;        (set ans fjm1)
;        (seq
;         (while (> n 1)
;            (set t fj)
;            (set fj (+ fj fjm1))
;            (set fjm1 t)
;            (set n (- n 1)))
;         (set ans fj)))
;   (print ans)))
;(compile-simp p6)

;(define p7 '(vars [(n 10) (fj 1) (fjm1 0) (t 0)]
;   (while (not (= n 1))
;      (set t fj)
;     (set fj (+ fj fjm1))
;      (set fjm1 t)
;      (set n (- n 1)))))
;(compile-simp p7)

;(define p4 '(vars [(n 10) (fj 1) (fjm1 0) (ans 0)]
;   (iif (= n 0)
;        (set ans fjm1)
;       (seq
;         (while (> n 1)
;            (set fjm1 fj)
;            (set fj (+ fj fjm1))
;            (set n (- n 1)))
;         (set ans fj)))
;   (print ans)))
;(compile-simp p4)

;(define p2 '(vars [(x 10) (y 1)]
;  (while (> x 0)
;     (set y (* 2 y))
;     (set x (- x 1)))
;  (print y)))
;(compile-simp p2)

;(define p1 '(vars [(x 0) (y 1) (z 2)] (while (> x 0)
;  (set y (* 2 y))
;  (set x (- x 1)))
;  (print y)))
;(compile-simp p1)

;(define r '(vars [(x 0) (y 1) (z 2)] (set x (+ (* y z) 2))
;                 (iif (or (< x y) (< x z)) (seq (print "do the thing")
;                                                (print "win the points"))
;                      (print "it's sports"))))
;(compile-simp r)

;(define r '(vars [(x 0) (y 2)] (seq (print "aoeu") (print (+ x y)))))
;(compile-simp r)

;(define p9 '(vars [(x 3) (y 4) (z 5)] (print (mod (mod x y) z))))
;(compile-simp p9)

;(define q '(vars [] (iif (or (and true false) (or (or (< 3 4) (= 4 4)) (= 4 (mod 3 4)) (= 9 10)))
;                         (print "blah") (print "meow"))))
;(compile-simp q)

(define m '(vars [] (iif (> 1 (+ 0 1)) (print "yes") (print "no"))))
(compile-simp m)