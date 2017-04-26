#lang racket

(require test-engine/racket-tests)

(struct bin (op fst snd) #:transparent) ; op is a symbol; fst, snd are ASTs.

(struct fun (param body) #:transparent) ; param is a symbol; body is an AST.

(struct app (fn arg) #:transparent) ; fn and arg are ASTs.

(struct seq (fst snd) #:transparent)

(struct set (var newval) #:transparent)

;; An AST is a (union bin fun app set seq).

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
    [`(,f ,x) (app (parse f) (parse x))]
    [`(seq ,x ,y) (seq (parse x) (parse y))]
    [`(set ,x ,y) (set x (parse y))]
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

;; interp: AST env store -> res
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
    [(set n v) (define v-eval (interp v env store))
               (result (void) (cons (list (lookup-env n env)
                                          (result-val v-eval))
                                    (result-newstore v-eval)))]
          ;(interp bdy (cons (sub v (interp arg-exp env))(conscl-env))])]
    [(bin op x y) (define x1 (interp x env store)) 
                  (define y1 (interp y env (result-newstore x1)))
                  (result ((op-trans op) (result-val x1)
                                         (result-val y1))
                          (result-newstore y1))]
    [(seq exp1 exp2) 
     (match (interp exp1 env store)
       [(result x s) (interp exp2 env s)])]
    [x (if (number? x)
           (result x store)
           (result (lookup x env store) store))]))
             
; completely inadequate tests
(check-expect (parse '(* 2 3)) (bin '* 2 3))

(check-expect (interp (parse '(* 2 3)) empty empty)
              (result 6 empty))

(check-expect (interp (parse '(with ((x 0)) (seq (seq (set x 1) 2)
                                                 (+ x 4)))) empty empty)
              (result 5 '((0 1) (0 0))))

(check-expect (interp (parse '(with ((x 0)) (+ (seq (set x 5) x)
                                               (seq (set x 6) x))))
                      empty empty) (result 11 '((0 6) (0 5) (0 0))))

(check-expect (interp (parse '(with ((x 0)) (+ (seq (set x 5) x)
                                               (seq (with ((x 10)) 0)
                                                    x)))) empty empty)
              (result 10 '((2 10)(0 5) (0 0))))
(test)
