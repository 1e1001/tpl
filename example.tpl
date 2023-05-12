#lang tpl racket/base
(require racket/format)
(define hello "World")
@tpl[(output/call displayln)]{
 this is a test tpl file
 @:when[(equal? hello "World")]{
  Hello, World!
 }
 @:unless[(equal? hello "World")]{
  Hello, someone else! (@~s[hello])
 }
}
