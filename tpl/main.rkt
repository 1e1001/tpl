#lang racket/base

(module reader syntax/module-reader
  tpl
  #:read scribble:read-inside
  #:read-syntax scribble:read-syntax-inside
  #:whole-body-readers? #t
  (require (prefix-in scribble: scribble/reader)))

(require racket/file
         racket/format
         racket/path
         racket/string
         (for-syntax
          racket/base))
(provide (except-out
          (all-from-out racket/base)
          #%module-begin)
         (rename-out
          [-module-begin #%module-begin])
         tpl-file
         tpl-output
         tpl-run
         tpl-run*
         output/file)

; {define-syntax-rule (-module-begin . body)
;   {#%module-begin
;     (define #%tpl-script-out #f)
;     (provide #%tpl-script-out)
;     (set! #%tpl-script-out (tpl-file . body))}}

; TODO: figure out this shit
; ideally it should push all the module-level forms to not count as a result item
{define-syntax (-module-begin stx)
  {parameterize ([current-namespace (make-base-namespace)])
    (define exprs (cdr (syntax-e (expand #`(begin . #,(syntax-e (cdr (syntax-e stx))))))))
    (define push-len 0)
    (define res
      {for/list ([expr exprs])
        (if {let/cc brk
              {unless (list? expr)
                (brk #f)}
              (define name (car expr))
              (or (free-identifier=? name #'#%expression)
                  (free-identifier=? name #'#%provide)
                  (free-identifier=? name #'#%declare)
                  (free-identifier=? name #'module)
                  (free-identifier=? name #'module*)
                  (free-identifier=? name #'module+)
                  (free-identifier=? name #'define-values)
                  (free-identifier=? name #'define-syntaxes)
                  (free-identifier=? name #'#%require))}
            expr
            {begin0
              #`(vector-set! res #,push-len #,expr)
              (set! push-len (add1 push-len))})})
    #`{#%module-begin
       (define #%tpl-script-out #f)
       (provide #%tpl-script-out)
       (define res (vector #,push-len))
       #,@res
       (set! #%tpl-script-out (apply tpl-file res))}}}

; convert an @-exp list into a string
{define (stringify item)
  (cond
    [(string? item) item]
    [(void? item) ""]
    [(list? item)
     (apply string-append (map stringify item))]
    [else (~s item)])}

(define output-func (make-parameter #f))

; parameter wrapper that takes multiple args
{define (tpl-output . args)
  (if (eq? args null)
      (output-func)
      (output-func args))}

{define-syntax-rule (tpl-file . body)
  {parameterize ([output-func #f])
    (define res (stringify (list . body)))
    (define out (output-func))
    {unless out
      (error "tpl-file call did not specify an output")}
    (apply (car out) res (cdr out))}}

{define (output/file res path)
  (error "todo")}

{define (tpl-run path)
  {define (collect-paths path)
    (case (file-or-directory-type path)
      [(file link) (if (path-has-extension? path #".rkt") path '())]
      [(directory directory-link)
       (map collect-paths
            (directory-list path #:build? #t))])}
  {define (inner-run path)
    (cond
      [(list? path) (map inner-run path)]
      [path
       (if (call-with-input-file
           path
           {λ (port)
             (define lang (read-language port {λ () #f}))
             (and lang (eq? (lang 'module-language #f) 'tpl))})
           (dynamic-require (make-resolved-module-path (normalize-path path)) '#%tpl-script-out)
           #f)]
      [else #f])}
  {define (clean-res res)
    (if (list? res)
        (map clean-res (filter values res))
        res)}
  (clean-res (inner-run (collect-paths path)))}

{define (tpl-run* . paths)
  (map tpl-run paths)}
