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
          [-module-begin #%module-begin]
          [tpl-doc ~*])
         tpl-file
         tpl-output
         tpl-run
         tpl-run*
         #%tpl-lift
         output/file
         ~when
         ~unless
         ~)

(struct tpl-doc (v) #:transparent)

{define (~ . args)
  (tpl-doc args)}

{define-syntax-rule (#%tpl-lift . body)
  {begin . body}}

; {define-syntax-rule (-module-begin . body)
;   {#%module-begin
;     (define #%tpl-script-out #f)
;     (provide #%tpl-script-out)
;     (set! #%tpl-script-out (tpl-file . body))}}

; TODO: figure out this shit
; ideally it should push all the module-level forms to not count as a result item
{define-syntax (-module-begin stx)
  {parameterize ([current-namespace (make-base-namespace)])
    (define exprs (syntax-e (cdr (syntax-e stx))))
    (define body-exprs null)
    (define item-exprs null)
    {for/list ([expr exprs])
      (if {let/cc brk
            (define expanded (syntax-e (expand expr)))
            {unless (pair? expanded)
              (brk #f)}
            (define name (syntax-e (car expanded)))
            (or (eq? name '#%expression)
                (eq? name '#%provide)
                (eq? name '#%declare)
                (eq? name 'module)
                (eq? name 'module*)
                (eq? name 'module+)
                (eq? name 'define-values)
                (eq? name 'define-syntaxes)
                (eq? name '#%require)
                (and (eq? name '#%app)
                     (syntax? (cdr expanded))
                     {let ([in (syntax-e (cdr expanded))])
                       (and (pair? in)
                            (pair? (car in))
                            (eq? (caar in) '#%top)
                            (eq? (cdar in) '#%tpl-lift))}))}
          (set! body-exprs (cons expr body-exprs))
          (set! item-exprs (cons expr item-exprs)))}
    #`{#%module-begin
       (define #%tpl-script-out #f)
       (provide #%tpl-script-out)
       #,@(reverse body-exprs)
       (set! #%tpl-script-out (tpl-file . #,(reverse item-exprs)))}}}

; convert an @-exp list into a string
{define (stringify item)
  (cond
    [(string? item) item]
    [(void? item) ""]
    [(tpl-doc? item)
     (apply string-append
            {for/list ([i (tpl-doc-v item)])
              (stringify i)})]
    [else (~s item)])}

(define output-func (make-parameter #f))

; parameter wrapper that takes multiple args
{define (tpl-output . args)
  (if (eq? args null)
      (output-func)
      (output-func args))}

{define-syntax-rule (tpl-file . body)
  {parameterize ([output-func #f])
    (define res (stringify (~ . body)))
    (define out (output-func))
    {unless out
      (error "tpl-file call did not specify an output")}
    (apply (car out) res (cdr out))}}

{define (output/file res path)
  (call-with-output-file
   path #:exists 'truncate/replace
   {λ (port)
     (display res port)})}

{define (tpl-run path)
  {define (collect-paths path)
    (case (file-or-directory-type path)
      [(file link)
       (if (and (path-has-extension? path #".rkt")
                (call-with-input-file
                 path
                 {λ (port)
                   (define lang (read-language port {λ () #f}))
                   (and lang (eq? (lang 'module-language #f) 'tpl))}))
           path
           '())]
      [(directory directory-link)
       (filter {λ (v)
                 (not (null? v))}
               (map collect-paths
                    (directory-list path #:build? #t)))])}
  {define (inner-run path)
    (cond
      [(null? path) #f]
      [(list? path) (map inner-run path)]
      [else (cons path (dynamic-require (make-resolved-module-path
                                         (normalize-path path))
                                        '#%tpl-script-out
                                        {λ () #f}))])}
  (inner-run (collect-paths path))}

{define (tpl-run* . paths)
  (map tpl-run paths)}

{define-syntax-rule (~when cond . args)
  (if cond (list . args) null)}
{define-syntax-rule (~unless cond . args)
 (if cond null (list . args))}

