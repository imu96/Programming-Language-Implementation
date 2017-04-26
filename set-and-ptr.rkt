#lang racket

(require test-engine/racket-tests)

(struct bin (op fst snd) #:transparent) ; op is a symbol; fst, snd are ASTs.

(struct fun (param body) #:transparent) ; param is a symbol; body is an AST.

(struct app (fn arg) #:transparent) ; fn and arg are ASTs.

(struct seq (fst snd) #:transparent)

(struct newbox (exp) #:transparent)
(struct openbox (exp) #:transparent)
(struct setbox (bexp vexp) #:transparent)

;; An AST is a (union bin fun app seq).

(struct sub (name val) #:transparent)

;; A substitution is a (sub n v), where n is a symbol and v is a number.
;; An environment (env) is a list of substitutions.
;; A memory is a pair of numbers

(struct closure (var body envt) #:transparent)

;; A closure is a (closure v bdy env), where
;; v is a symbol, bdy is an AST, and env is a environment.

(struct result (val newstore) #:transparent)
;; A result is a (result v s), where s is a list of memories,
;;     and v is a (union closure number)
;; parse: sexp -> AST

(define (parse sx)
  (match sx
    [`(with ((,nm ,nmd)) ,bdy) (app (fun nm (parse bdy)) (parse nmd))]
    [`(+ ,x ,y) (bin '+ (parse x) (parse y))]
    [`(* ,x ,y) (bin '* (parse x) (parse y))]
    [`(- ,x ,y) (bin '- (parse x) (parse y))]
    [`(/ ,x ,y) (bin '/ (parse x) (parse y))]
    [`(fun (,x) ,bdy) (fun x (parse bdy))]
    [`(seq ,x ,y) (seq (parse x) (parse y))]
    [`(box ,x) (newbox (parse x))]
    [`(unbox ,x) (openbox (parse x))]
    [`(setbox ,x ,y) (setbox (parse x) (parse y))]
    [`(,f ,x) (app (parse f) (parse x))]
    [x x]))

; op-trans: symbol -> (number number -> number)
; converts symbolic representation of arithmetic function to actual Racket function
(define (op-trans op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]))

;; lookup: symbol env -> (union number fun)
;; looks up a substitution in an environment (topmost one)

(define (lookup-env var env)
  (cond
    [(empty? env) (error 'interp "unbound variable ~a" var)]
    [(symbol=? var (sub-name (first env))) (sub-val (first env))]
    [else (lookup-env var (rest env))]))

(define (lookup-mem var env)
  (cond
    [(empty? env) (error 'interp "unbound variable ~a" var)]
    [(= var (first (first env))) (second (first env))]
    [else (lookup-mem var (rest env))]))

(define (lookup x env store)
  (lookup-mem (lookup-env x env) store))

;; interp: AST env -> (val newstore)
(define (interp ast env store)
  (match ast
    [(fun v bdy) (result (closure v bdy env) store)]
    [(app fun-exp arg-exp) 
     (define fun-eval (interp fun-exp env store))
     (define arg-eval (interp arg-exp env 
                              (result-newstore fun-eval)))
       (match fun-eval
         [(result (closure v bdy cl-env) cl-store)
          (define newloc (length (result-newstore arg-eval)))
          (interp bdy (cons (sub v newloc) cl-env)
                  (cons (list newloc (result-val arg-eval))
                        (result-newstore arg-eval)))])]
    [(bin op x y) (define x1 (interp x env store)) 
                  (define y1 (interp y env (result-newstore x1)))
                  (result ((op-trans op) (result-val x1)
                                         (result-val y1))
                          (result-newstore y1))]
    [(seq exp1 exp2) 
     (match (interp exp1 env store)
       [(result x s) (interp exp2 env s)])]
    [(newbox exp)
     (match (interp exp env store)
       [(result v s) (define loc (length s))
                     (result loc (cons `(,loc ,v) s))])]
    [(openbox exp)
     (match (interp exp env store)
       [(result v s) (result (lookup-mem v s)
                             s)])]
    [(setbox exp1 exp2)
     (match (interp exp1 env store)
       [(result v1 s1)
        (match (interp exp2 env s1)
          [(result v2 s2) 
           (result (void) (cons (list v1 v2) s2))])])]
    [x (if (number? x)
           (result x store)
           (result (lookup x env store) store))]))
             
(check-expect (parse '(* 2 3)) (bin '* 2 3))

(check-expect (interp (parse '(* 2 3)) empty empty)
              (result 6 empty))

(check-expect (parse '(box 4)) (newbox 4))
(check-expect (parse '(unbox 0)) (openbox 0))
(check-expect (parse '(setbox 0 5)) (setbox 0 5))
(check-expect (parse '(seq (box 4) (unbox 0))) 
              (seq (newbox 4) (openbox 0)))

(check-expect (interp (parse '(box 4)) empty empty)
              (result 0 '((0 4))))

(check-expect (interp (openbox 0) empty '((0 4)))
              (result 4 '((0 4))))

(check-expect (interp (seq (newbox 4) (openbox 0)) empty empty)
              (result 4 '((0 4))))
;(define blah '(seq (box 4) (unbox 0)))
;(check-expect (interp (parse blah) empty empty)
 ;             (result 4 '((0 4 ))))
(check-expect (interp (parse '(seq (box 4) (unbox 0))) empty empty)
              (result 4 '((0 4))))

(test)
