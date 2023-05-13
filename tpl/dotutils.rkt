#lang racket/base
; tpl/dotutils - utilities for use as a configuration manager
(require tpl
         file/glob
         racket/format
         racket/string)
{define+provide (parse-command-line-arguments [args (current-command-line-arguments)])
  {for/hash ([i args])
    (define split (string-split i "=" #:trim? #f))
    (if (<= (length split) 1)
      (error "invalid argument" i)
      (values (string->symbol (car split))
              (read (open-input-string (string-join (cdr split) "=")))))}}

{define+provide (output/if-newer path inner)
  (if (> (file-or-directory-modify-seconds (current-tpl-script))
         (file-or-directory-modify-seconds path #f {λ () 0}))
      (inner path)
      ; returning `void` instead of `(void)` is intentional, `void` is a "valid" output
      void)}

{define (inner-launch #:stdio [stdio '(#f #f #f)] #:during [during void] #:after [after void] . exec)
  (define-values (subproc stdout stdin stderr)
    (apply
     subprocess
     (if (car stdio) #f (current-output-port))
     (if (cadr stdio) #f (current-input-port))
     (if (caddr stdio) #f (current-error-port))
     exec))
  (during subproc stdout stdin stderr)
  {when (car stdio) (close-input-port stdout)}
  {when (cadr stdio) (close-output-port stdin)}
  {when (caddr stdio) (close-input-port stderr)}
  (subprocess-wait subproc)
  (after subproc)
  {unless (eq? (subprocess-status subproc) 0)
    (error "system call failed")}
}

{define+provide ((output/file-as-root path) body)
  (case (system-type 'os)
    [(unix macos)
     (printf "→ root: write file ~a\n" path)
     (inner-launch
      (find-executable-path "sudo")
      (find-executable-path "dd")
      (bytes-append #"of=" (path->bytes (if (path? path) path (string->path path))))
      #:stdio '(#f #t #f)
      #:during {λ (_0 _1 stdin _2)
             (display (tpl-doc->string (body)) stdin)}
      #:after {λ (_0) (printf "← root…\n")})]
    [else (error "output/file-as-root is only supported on unix, got" (system-type 'os))])}

{define (resolve-command-like cmd)
  (cond
    [(path? cmd) cmd]
    [(string? cmd) (find-executable-path cmd)]
    [(list? cmd) (car cmd)])}

{define (resolve-exec-help help args)
  (case help
    [(#f) (printf "~a…\n" (car args))]
    [(#t) (printf "~a\n" (string-join (map ~a args) " "))]
    [else (printf "~a\n" help)])}

{define+provide (exec-as-root help cmd . args)
  (printf "→ root: ")
  (resolve-exec-help help (cons cmd args))
  (apply
   inner-launch
   (find-executable-path "sudo")
   (resolve-command-like cmd)
   args
   #:after {λ _ (printf "← root…\n")})}

{define+provide (exec help cmd . args)
  (printf "→ exec: ")
  (resolve-exec-help help (cons cmd args))
  (apply
   inner-launch
   (resolve-command-like cmd)
   args
   #:after {λ _ (printf "← exec…\n")})}

{define+provide (tpl-run-glob pattern)
  (map tpl-run (glob pattern #:capture-dotfiles? #t))}
