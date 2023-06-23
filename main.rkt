#lang racket
(require (for-syntax syntax/parse racket))

(provide term define-ftg-interface define-ftg-eval
         combine-eval combine-eval/instantiate)
(define-for-syntax (determ stx ctx)
  (syntax-parse stx
    #:literals (unquote)
    [(unquote t) #'(t ctx)]
    [(f args ...)
     #:when (equal? (syntax-property stx 'paren-shape) #\{)
     #'((term (f args ...)) ctx)]
    [(f args ...)
     #'(f args ...)]
    [lit #'lit]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ (f args ...))
     #:with (args- ...) (datum->syntax #'ctx
                                       (map (λ (stx) (determ stx #'ctx))
                                            (syntax-e #'(args ...))))
     #`(λ (ctx)
         (send ctx f args- ...))]))

(begin-for-syntax
  (struct ftg-static-info (definition transformer)
  #:property prop:procedure (struct-field-index transformer)))

(define-syntax (define-ftg-interface stx)
  (syntax-parse stx
    [(_ name:id (tag field:id ...) ...)
     #`(begin
         (define name-interface
           (interface ()
             tag ...))
         (define-syntax name
           (ftg-static-info (quote-syntax ((tag field ...) ...))
                            (λ (stx)
                              (syntax-parse stx
                                [x:id (quote-syntax name-interface)])))))]))

(define-for-syntax (check-tags tags definitions who)
  (define tags-set (list->seteq tags))
  (define def-tags
    (syntax-parse definitions
      [((tag field ...) ...) (syntax->datum #'(tag ...))]))
  (let loop ([def-tags def-tags][tags-set tags-set])
    (if (null? def-tags)
        (unless (set-empty? tags-set)
          (define tags (set->list tags-set))
          (define tags-str (string-join (map symbol->string tags) ","))
          (raise-syntax-error 'define-ftg-eval (string-append "unexpected cases in evaluator: " tags-str) who))
        (let ([tag (car def-tags)]
              [rst (cdr def-tags)])
          (if (set-member? tags-set tag)
              (loop rst (set-remove tags-set tag))
              (raise-syntax-error 'define-ftg-eval (string-append "unbound case in evaluator: " (symbol->string tag)) who))))))

(define-syntax (define-ftg-eval stx)
  (syntax-parse stx
    [(_ evaluator:id ftg-interface:id [(tag field:id ...) bodies ...] ...)
     #:do [(define interface-definitions (ftg-static-info-definition (syntax-local-value #'ftg-interface)))
           (check-tags (syntax->datum #'(tag ...)) interface-definitions #'evaluator)]
     #`(begin
         (define evaluator
           (mixin () (ftg-interface)
             (super-new)
             (define/public (tag field ...)
               bodies ...)
             ...)))]))

(define (combine-eval . evaluators)
  (define base-mixin (λ (x) x))
  (foldr (λ (f g) (compose f g)) base-mixin evaluators))

(define (combine-eval/instantiate . evaluators)
  (define evaluator-mixin (apply combine-eval evaluators))
  (define inst (new (evaluator-mixin object%)))
  (λ (t) (t inst)))






