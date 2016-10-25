' ------------------------------------------------------------------------------
' -- sodaware.lispmax
' -- 
' -- A terrible LISP implementation written in BlitzMax. Following along with 
' -- the "Building Lisp" tutorial here: http://www.lwh.jp/lisp/
' ------------------------------------------------------------------------------


Module sodaware.lispmax

SuperStrict

' Import core files
Import "src/lispmax_lexer.bmx"
Import "src/lispmax_exceptions.bmx"
Import "src/lispmax_core.bmx"
Import "src/lispmax_repl.bmx"

' Include lisp library code
Incbin "site-lisp/library.lisp"
