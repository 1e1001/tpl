#lang racket/base
; tpl - main entry point & lang
(require racket/format
         racket/path
         racket/file)

; this is functionally equivalent to doing
;   #lang at-exp <lang>
;   (require tpl)
;   ...
; but cooler (allows lang-detection in tpl-run)
(module reader syntax/module-reader
  #:language read
  #:info
  {λ (key default parent)
    (if (eq? key 'tpl-script?)
        #t
        (parent key default))}
  #:wrapper2
  {λ (in rd stx?)
    {parameterize ([current-readtable
                    (make-at-readtable)])
      (define mod (rd in))
      (define mod-e (syntax-e mod))
      (define beg (cadddr mod-e))
      (define beg-e (syntax-e beg))
      (datum->syntax
       mod
       (list (car mod-e)
             (cadr mod-e)
             (caddr mod-e)
             (datum->syntax
              beg
              (list* (car beg-e)
                     ; #'(require tpl) would be more correct here,
                     ; but that results in an ambiguous identifier or something
                     (datum->syntax beg '(require tpl))
                     (cdr beg-e))
              beg))
       mod)}}
  (require scribble/reader))

; define & provide in the same expression
(provide define+provide)
{define-syntax define+provide
  (syntax-rules ()
    [(_ (name . args) . body)
     (define+provide name (λ args . body))]
    [(_ name value)
     {begin
       (provide name)
       (define name value)}])}
(provide define+provide-syntax-rule)
{define-syntax-rule (define+provide-syntax-rule (name . args) body)
  {begin
    (provide name)
    (define-syntax-rule (name . args) body)}}

; document type
(provide (struct-out tpl-doc))
(struct tpl-doc (v) #:transparent)
{define+provide (: . args)
  (tpl-doc args)}

; convert an @-exp list into a string
{define+provide (tpl-doc->string item)
  (cond
    [(string? item) item]
    [(void? item) ""]
    [(tpl-doc? item)
     (apply string-append (map tpl-doc->string (tpl-doc-v item)))]
    [else (~s item)])}

; "@:" expressions are like the normal ones but specifically for tpl-doc's
{define+provide-syntax-rule (define/@:expr name fn args ...)
  {define-syntax-rule (name args ... . body)
    (fn args ... (: . body))}}
{define+provide-syntax-rule (define+provide/@:expr name fn args ...)
  {begin
    (provide name)
    (define/@:expr name fn args ...)}}
; some useful ones from racket/base
(define+provide/@:expr :when when cond)
(define+provide/@:expr :unless unless cond)
(define+provide/@:expr :parameterize parameterize vars)

; different types of output
; in the format of ((output/whatever . args) body) or (output/whatever body)
{define+provide-syntax-rule (output/module body)
  (define+provide #%tpl-output/module (body))}
{define+provide ((output/file path) body)
  (printf "→ write file ~a\n" path)
  (make-directory* (path-only path))
  (call-with-output-file
   path #:exists 'truncate/replace
   {λ (port)
     (display (tpl-doc->string (body)) port)})
  (printf "← write file…\n")}
{define+provide ((output/call fn . args) body)
  (apply fn (tpl-doc->string (body)) args)}

; defines a document
{define+provide-syntax-rule (tpl out-fn . body)
  (out-fn {λ () (: . body)})}

; current executing tpl script
(define+provide current-tpl-script (make-parameter (find-system-path 'run-file) #f 'current-tpl-script))

; recurse tpl scripts
{define+provide (tpl-run path)
  (define paths
    {let collect-paths ([path path])
      (case (file-or-directory-type path)
        [(file link)
         (if (and (or (path-has-extension? path #".rkt")
                      (path-has-extension? path #".tpl"))
                  (call-with-input-file
                   path
                   {λ (port)
                     (define lang (read-language port {λ () #f}))
                     (and lang (lang 'tpl-script? #f))}))
             path
             '())]
        [(directory directory-link)
         (filter {λ (v)
                   (not (null? v))}
                 (map collect-paths
                     (directory-list path #:build? #t)))])})
  {let inner-run ([path paths])
    (cond
      [(null? path) #f]
      [(list? path) (map inner-run path)]
      [else
       (define mod-path (make-resolved-module-path (normalize-path path)))
       {parameterize ([current-tpl-script path])
         (dynamic-require
          mod-path
          0
          {λ () #f})
         (cons path (dynamic-require
                     mod-path
                     '#%tpl-output/module
                     {λ () #f}))}])}}
{define+provide (tpl-run* . paths)
  (map tpl-run paths)}
