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
         racket/string)
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

{define-syntax-rule (-module-begin . body)
  {#%module-begin
    (define #%tpl-script-out (tpl-file . body))
    (provide #%tpl-script-out)}}

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
       (filter {λ (v) (not (null? v))}
               (map collect-paths
                    (directory-list path #:build? #t)))])}
  (writeln (collect-paths path))}

{define (tpl-run* . paths)
  (map tpl-run paths)}
