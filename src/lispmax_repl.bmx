SuperStrict

Import sodaware.Console_Color

Import "lispmax_core.bmx"

Type Lispmax_REPL
	
	Field _lisp:LispMax
	
	Method addFunction(name:String, fn:LispMax_Atom)
		Self.getEnvironment().set(Self.getLisp().makeSymbol(name), fn)
	End Method
	
	Method getLisp:LispMax()
		Return Self._lisp
	End Method
	
	Method getEnvironment:LispMax_Environment()
		Return Self._lisp._environment
	End Method
	
	Method run()
	
		Local content:String
			
		' REPL
		Repeat
			
			content = Input("> ")
			If content = "exit" Then End
			
			content = content + "~0"
			
			?Not Debug
			Try
				Local expression:LispMax_Atom = Self._lisp.parseExpression(content)
				WriteC "=> " 
				Self._lisp.printExpression( Self._lisp.evaluateExpression(expression, Self._lisp._environment))
				WriteC "~n"
			Catch e:Lispmax_SyntaxErrorException
				PrintC "%rSyntax error: %n" + e.ToString()
			Catch e:Lispmax_ArgumentException
				PrintC "%rArgument error: %n" + e.ToString()
			Catch e:Lispmax_UnexpectedTypeException
				PrintC "%rUnexpected type error: %n" + e.ToString()
			Catch e:Lispmax_UnboundSymbolException
				PrintC "%rUnbound symbol: %n" + e.ToString()
			End Try
			?Debug
				Local expression:LispMax_Atom = Self._lisp.parseExpression(content)
				WriteC "=> " 
				Self._lisp.printExpression( Self._lisp.evaluateExpression(expression, Self._lisp._environment))
				WriteC "~n"			
			?
		
		Until content = ""
		
	End Method
	
	Method New()
		Self._lisp	= New LispMax
		Self._lisp.initializeEnvironment()
	End Method
	
End Type