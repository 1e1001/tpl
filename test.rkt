#lang tpl
@require[racket/format]
@#%tpl-lift[(displayln "this expression is lifted")]
@tpl-output[displayln]
this is a test tpl file
@~v[@tpl-run["."]]
