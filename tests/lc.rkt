#lang racket
(require "../main.rkt")
(define-ftg-interface λSym
  (lit c)
  (app f x)
  (λ var body)
  (var n))

(define-ftg-eval λSym-print λSym
  [(lit c) c]
  [(app f x) `(,f ,x)]
  [(λ var body) `(λ (,var) ,body)]
  [(var n) n])

(define print (combine-eval/instantiate λSym-print))

(define t1 (term {λ 'x {var 'x}}))
(define t2 (term {app ,t1 ,t1}))
(print t1)
(print t2)


(define-ftg-eval λSym-eval λSym
  [(lit c) (lambda (env) c)]
  [(app f x) (lambda (env)
               ((f env) (x env)))]
  [(λ var body) (lambda (env)
                  (lambda (v)
                    (body `((,var . ,v) . ,env))))]
  [(var n) (lambda (env)
             (cdr (assq n env)))])

(define t3 (term (lit 1)))
(define t4 (term {app ,t2 ,t3}))
(define eval (combine-eval/instantiate λSym-eval))
((eval t4) '())


