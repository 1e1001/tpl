#lang tpl
@require[racket/format]
@require["conftest.rkt"]
@tpl-output[displayln]
this is a test tpl file
@~when[(equal? hello "World")]{
  Hello, World!
}
