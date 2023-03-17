#!/usr/bin/env racket
#lang racket/base
(require racket/format
         racket/list
         racket/string
         (for-syntax
          racket/base
          racket/string))
(provide ~string-out
         ~file-out
         ~std-out
         stringify
         ~when
         ~unless
         load-args)
{define-syntax-rule (~string-out . args)
  (string-join (map stringify (flatten (list . args))) "")}
{define-syntax-rule (~std-out . args)
  (displayln (~string-out . args))}
{define-syntax-rule (~file-out path . args)
  {when path
    (call-with-output-file
     path
     {lambda (out)
       (display (~string-out . args) out)}
     #:exists 'truncate/replace)}}
{define (stringify v)
  (if (string? v)
      v
      (~s v))}
{define-syntax-rule (~when cond . args)
  (if cond (list . args) null)}
{define-syntax-rule (~unless cond . args)
  (if cond null (list . args))}
{define-syntax-rule (load-args id)
  (inner-load-args id)}
{define-syntax (inner-load-args stx)
  (define prefix (cadr (syntax-e stx)))
  (define prefix-str (symbol->string (syntax-e prefix)))
  (define prefix-len (string-length prefix-str))
  {define (build-id name)
    (datum->syntax
     prefix
     (string->symbol
      (string-append
       prefix-str
       name))
     prefix)}
  (define vars
    {for/hash ([arg (current-command-line-arguments)]
                #:when (and (>= (string-length arg) prefix-len)
                            (equal? (substring arg 0 prefix-len) prefix-str)))
      (define split (string-split (substring arg prefix-len) "=" #:trim? #f))
      {when (or (< (length split) 2)
                (equal? (car split) ""))
        (error "invalid empty argument")}
      (values (car split)
              (string-join (cdr split) "="))})
  #`{begin
     #,@{for/list ([(k v) vars])
          #`(define #,(build-id k) #,v)}
     (define #,prefix #,vars)}}
(module+ main
  (displayln "running all items"))
