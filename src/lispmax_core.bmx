' ------------------------------------------------------------------------------
' -- src/lispmax_core.bmx
' --
' -- The core of lispmax. Contains the main LispMax type which contains a lisp
' -- environment, execution stack and a symbol table.
' --
' -- This file is part of sodaware.mod (https://www.sodaware.net/sodaware.mod/)
' -- Copyright (c) 2009-2017 Phil Newton
' --
' -- See LICENSE for full license information.
' ------------------------------------------------------------------------------


SuperStrict

Import brl.map
Import brl.filesystem
Import brl.ramstream
Import brl.random

Import "lispmax_exceptions.bmx"
Import "lispmax_lexer.bmx"

Include "lispmax_process.bmx"
Include "lispmax_environment.bmx"
Include "lispmax_stackframe.bmx"
Include "lispmax_atom.bmx"
Include "lispmax_atommap.bmx"
Include "lispmax_builtin.bmx"

' Set up globals
Global LispMax_Nil:LispMax_Atom


Type LispMax
	Field _environment:LispMax_Environment  '< The environment expressions are being evaluated in.
	Field _expression:LispMax_Atom          '< The current expression being evaluated.
	Field _stack:Lispmax_StackFrame         '< Stack of LispMax_Stack objects.
	Field _result:LispMax_Atom              '< Result from the last evaluated expression.

	Field _lexer:LispMax_Lexer              '< The internal lexer for lexing lisp expressions.
	Field symbolTable:LispMax_AtomMap       '< The internal symbol table.

	' Arg reader states.
	Const ARG_STATE_NORMAL:Byte = 1         '< Standard arg reading.
	Const ARG_STATE_REST:Byte   = 2         '< Load rest of args into a single list.
	Const ARG_STATE_KEY:Byte    = 2         '< Load args into keywords.

	' ----------------------------------------------------------------------
	' -- Evaluating Expressions
	' ----------------------------------------------------------------------

	''' <summary>
	''' Evaluates a parser expression atom within an environment and returns
	''' the resulting atom.
	'''
	''' This is the usual entry-point for evaluating a parsed expression.
	''' </summary>
	''' <param name="expression">The parsed expression to evaluate.</param>
	''' <param name="environment">The environment to evaluate the expression within.</param>
	''' <return>The resulting atom.</return>
	Method evaluateExpression:LispMax_Atom(expression:LispMax_Atom, environment:LispMax_Environment = Null)

		' Load the environment and set the expression
		If environment Then Self._environment = environment
		Self._expression = expression

		Local isFinished:Byte = False

		' Evaluate until stack is empty
		Repeat
			isFinished = Self.evaluateCurrentExpression()
		Until isFinished

		Return Self._result

	End Method


	' ----------------------------------------------------------------------
	' -- Evaluating Expressions (Process Mode)
	' ----------------------------------------------------------------------

	''' <summary>
	''' Creates a new LispMax_Process object, sets its environment and gives it
	''' an expression to evaluate. Evaluation does not take place until "start"
	''' is called on the process, and evaluation can be paused at any time.
	''' </summary>
	Method createProcess:LispMax_Process(expression:LispMax_Atom, environment:LispMax_Environment = Null)

		' Create a new process
		Local process:LispMax_Process = LispMax_Process.SpawnProcess(Self)

		' Allow environment to be passed in.
		If environment <> Null Then
			process._environment = LispMax_Environment.Create(environment)
		EndIf

		' Set the expression
		process._expression = expression

		' Return the process
		Return process

	End Method

	Method stopProcess()
		Throw "SUSPEND only works in process mode"
	End Method


	' ----------------------------------------------------------------------
	' -- Adding Functions
	' ----------------------------------------------------------------------

	''' <summary>
	''' Add an executable function to the Lisp environment. Adds the function
	''' as NAME that will execute the code in CALLBACK when called.
	''' </summary>
	''' <seealso cref="Lispmax_Callable">Base callable object.</seealso>
	''' <param name="name">The name of the function to add.</param>
	''' <param name="callback">A callable object to execute.</param>
	Method addFunction(name:String, callback:Lispmax_Callable)
		Assert name,     "Cannot add a function without a valid name"
		Assert callback, "Invalid function passed to `addFunction`"

		Self._environment.set(Self.makeSymbol(name), Self.makeCallable(callback))
	End Method


	' ----------------------------------------------------------------------
	' -- Querying
	' ----------------------------------------------------------------------

	''' <summary>Get the result from the last expression executed.</summary>
	Method getResult:LispMax_Atom()
		Return Self._result
	End Method


	' ----------------------------------------------------------------------
	' -- Moving between expressions
	' ----------------------------------------------------------------------

	''' <summary>Set the expression to an expression on the current stack.</summary>
	Method moveToNextExpression()

		' Update the current environment and get the body
		' TODO: This is fairly slow as the body node needs copying for EVERY expression.
		' TODO: Might not actually need to copy it? TEST!
		' TODO: Also, copying doesn't add too much overhead compared to some other parts.
		Self._environment       = Self._stack._environment
		Local body:LispMax_Atom = Self._stack._body.copy()

		' Update the current expression to match next body exp
		Self._expression = body.value_pair.atom[LISPMAX_CAR]
		body             = body.value_pair.atom[LISPMAX_CDR]

		' If no body expressions left, pop from the stack.
		If body.isNil() Then
			Self._stack = Self._stack._parent
		Else
			Self._stack._body = body
		End If

	End Method

	''' <summary>
	''' Binds function arguments into a new environment (if not already bound),
	''' and calls doExec to get the next expression in the body.
	''' </summary>
	Method bindCurrentArguments()

		' Check the stack is valid
		If Self._stack = Null Then Throw New LispMax_NullStackException

		' If body still has items left, get the next one
		If Not(Self._stack._body.isNil()) Then
			Self.moveToNextExpression()
			Return
		EndIf

		' Get the operator and its args
		Local op:LispMax_Atom        = Self._stack._op
		Local args:LispMax_Atom      = Self._stack._args
		Local body:LispMax_Atom      = Self._stack._body
		Local arg_names:LispMax_Atom

		' Create a new environment from this
		Self._environment = LispMax_Environment.Create(Self._stack._environment)

		' Get arg names
		arg_names = op.value_pair.atom[LISPMAX_CAR]
		body	  = op.value_pair.atom[LISPMAX_CDR]

		' Update stack pointer
		Self._stack._environment = Self._environment
		Self._stack._body        = body

		Local isFinished:Byte = False

		' Bind the arguments

		' TODO: This is very slow and not nicely written... Fix it.
		While False = isFinished

			' dotted arg?
			If arg_names.atom_type = LispMax_Atom.ATOM_TYPE_SYMBOL Then
				Self._environment.set(arg_names, args)
				args = LispMax_Nil
				Exit
			End If

			' If no args, quit
			If arg_names.car() = Null Then
				Exit
			End If

			' Check for special args.
			If arg_names.car().value_symbol = "&REST" Then

				' &REST must be the final keyword
				If Not(arg_names.cddr().isNil()) Then
					Throw New Lispmax_ArgumentException
				End If

				' Set the arg name to whatever the remaining args are
				Self._environment.set(arg_names.cdar(), Self.copyList(args))

				' Nil the args so we don't throw an error
				args = Self.makeNil()

				' Exit the loop
				isFinished = True
				Exit

			End If

			' Check for special args.
			If arg_names.car().value_symbol = "&KEY" Then

				' Update the state
				'arg_state = LispMax.ARG_STATE_KEY

				' Move past the keyword
				arg_names = arg_names.cdr()

				' TODO: This is only used internally, so we can probably create an AtomMap instead.
				' Get a list of all key names and defaults.
				Local keys:LispMax_Atom = Self.getExpressionKeys(arg_names)
				Local temp:LispMax_Atom = Self.copyList(keys)

				' Set all initial values values.
				Repeat
					Self._environment.set(temp.caar(), temp.cadr())
					temp = temp.cdr()
				Until temp.isNil()

				' Check args
				Local absorbed:Int = args.isNil()
				While absorbed     = False

					' Get the named arg
					Local current_arg_name:LispMax_Atom = args.car()
					args = args.cdr()

					If current_arg_name.atom_type = LispMax_Atom.ATOM_TYPE_SYMBOL
						If Self.listContainsValue(keys, current_arg_name.value_symbol.Replace(":", "")) Then

							' Set this value
							Self._environment.set(Self.Makesymbol(current_arg_name.value_symbol.Replace(":", "")), args.car())
							args = args.cdr()

						EndIf
					End If

					If args.isNil() Then
						absorbed = True
					End If

				Wend

				If args.isNil() Then Exit
				Continue

			End If

			' Not enough args sent
			If args.isNil()
				Throw New Lispmax_ArgumentException
			End If

			' Add arg to environment
			Self._environment.set(arg_names.car(), args.car())

			' Move to next argument
			arg_names = arg_names.cdr()
			args      = args.cdr()

			' Finish if no more arg_names remaining
			isFinished = arg_names.isNil()

		Wend

		' Too many args
		If args.isNil() = False Then
			Throw New Lispmax_ArgumentException
		End If

		' Clear waiting args from stack frame
		Self._stack._args = Self.makeNil()

		' Execute
		Self.moveToNextExpression()

	End Method

	''' <summary>
	''' Called once all arguments have been evaluated.
	'''
	''' Either sets the expression a builtin or a closure/user-defined function.
	''' </summary>
	Method doApply()
		Local op:LispMax_Atom   = Self._stack._op
		Local args:LispMax_Atom = Self._stack._args

		' Reverse order of args to evaluate.
		If args.isNil() = False Then
			args = Self.listReverse(args)
			Self._stack._args = args
		End If

		' Apply symbols
		If op.atom_type = LispMax_Atom.ATOM_TYPE_SYMBOL Then

			' TODO: This is not nice.
			If op.value_symbol = "APPLY" Then

				' debuglog "Applying"

				' Replace the current frame
				Self._stack = Self._stack._parent
				Self._stack = LispMax_StackFrame.Create(Self._stack, Self._environment, Self.makeNil())

				' Get operator / args from apply.
				op   = args.car()
				args = args.cdar()

				' If args is not a list, syntax is incorrect.
				If args.isList() = False Then
					Throw New Lispmax_SyntaxErrorException
				End If

				' Update stack.
				Self._stack._op   = op
				Self._stack._args = args

			End If

		ElseIf op.atom_type = LispMax_Atom.ATOM_TYPE_BUILTIN Then

			Self._stack      = Self._stack._parent
			Self._expression = Self.cons(op, args)

			Return
		ElseIf op.atom_type <> LispMax_Atom.ATOM_TYPE_CLOSURE Then

			Throw Lispmax_UnexpectedTypeException.Create(op.value_symbol, LispMax_Atom.atom_type_closure, op.atom_type)

		End If

		Self.bindCurrentArguments()

	End Method

	''' <summary>
	''' Called once an expression has been evaluated.
	'''
	''' Stores the result, which is either an operator, argument or body
	''' expression. Fetches next expression to evaluate.
	''' </summary>
	Method doReturn:LispMax_Atom()

		' Get the environment and operator/args from the stack
		Local op:LispMax_Atom   = Self._stack._op
		Local args:LispMax_Atom = Self._stack._args
		Local body:LispMax_Atom = Self._stack._body
		Self._environment       = Self._stack._environment

		' Apply the stack expression if it's not empty.
		If body.isNil() = False Then
			Self.doApply()

			Return Self._result
		EndIf

		' Finished!
		If op.isNil() Then

			op              = Self._result
			Self._stack._op = op

			' Don't evaluate macro arguments
			If LispMax_Atom.ATOM_TYPE_MACRO = op.atom_type Then

				' Copy the macro Atom.
				local macro:LispMax_Atom = op.copy()

				' Turn it into something executable.
				macro.atom_type = LispMax_Atom.ATOM_TYPE_CLOSURE

				' Set macro args.
				args = Self._stack._tail

				' Push to stack.
				Self._stack       = LispMax_StackFrame.Create(Self._stack, Self._environment, Self.makeNil())
				Self._stack._op   = macro
				Self._stack._args = args

				Self.bindCurrentArguments()

				Return Self._result

			EndIf

		ElseIf LispMax_Atom.ATOM_TYPE_SYMBOL = op.atom_type Then

			select op.special_symbol

				case LispMax_Atom.SYMBOL_DEFINE

					Local symbol:Lispmax_Atom = Self._stack._args
					Self._environment.set(symbol, Self._result)

					' Pop stack
					self._stack = Self._stack._parent
					Self._expression = Self.cons(Self.makeSymbol("QUOTE"), Self.cons(symbol, Self.makeNil()))

					Return Self._result

				case Lispmax_Atom.SYMBOL_IF

					args = Self._stack._tail

					If Self._result.isNil() Then
						Self._expression = args.cdar()
					Else
						Self._expression = args.car()
					End If

					Self._stack = Self._stack._parent

					Return Self._result

				Case LispMax_Atom.SYMBOL_WHEN

					args = Self._stack._tail

					If Self._result.isNil() Then
						Self._expression = LispMax.makeNil()
					Else
						Self._expression = Self.cons(Self.makeSymbol("PROGN"), args)
					End If

					Self._stack = Self._stack._parent

					Return Self._result

				Case LispMax_Atom.SYMBOL_UNLESS

					args = Self._stack._tail

					If Self._result.isNil() Then
						Self._expression = Self.cons(Self.makeSymbol("PROGN"), args)
					Else
						Self._expression = LispMax.makeNil()
					End If

					Self._stack = Self._stack._parent

					Return Self._result

				Default

					args = Self.storeArg(Self._result)

			End select

		ElseIf op.atom_type = LispMax_Atom.ATOM_TYPE_MACRO Then

			' Finished evaluating maco
			Self._expression = Self._result
			Self._stack = Self._stack._parent

			Return Self._result

		Else
			args = Self.storeArg(Self._result)
		EndIf

		args = Self._stack._tail

		' Check if anything is left to evaluate
		If args.isNil() Then
			Self.doApply()

			Return Self._result
		End If

		' Evaluate next expression
		Self._expression	= args.car()
		Self._stack._tail   = args.cdr()

		Return Self._result

	End Method


	' ----------------------------------------------------------------------
	' -- Internal expression evaluation
	' ----------------------------------------------------------------------

	Method evaluateCurrentExpression:Byte()

		' Check for symbols first.
		If LispMax_Atom.ATOM_TYPE_SYMBOL = Self._expression.atom_type Then
			' Check if the symbol starts with ":", which will evaluate to
			' itself. Otherwise return the symbol's value from the environment.
			If Self._expression.value_symbol[0] = ASC_COLON Then
				Self._result = Self._environment.getOrSet(Self._expression, Self._expression)
			Else
				' Set current result to the symbol's value.
				' This could be a built-in function or a callable function.
				Self._result = Self._environment.get(Self._expression)
			EndIf

		ElseIf Self._expression.atom_type <> LispMax_Atom.ATOM_TYPE_PAIR Then

			Self._result = Self._expression

			' TODO: This is SLOW on bigger expressions.
			' TODO: Restore this, but in a more efficient way.
'		ElseIf Self._expression.isList() = False Then
'			' [todo] - Include some extra information here
'			Throw New Lispmax_SyntaxErrorException
		Else
			' TODO: Would be nice to use bmk-ng here for operator overloading :(
			Local op:Lispmax_Atom   = Self._expression.car()
			Local args:Lispmax_Atom = Self._expression.cdr()

			' Check for special atoms
			If op.isSymbolAtom() Then

				Select op.special_symbol

					Case LispMax_Atom.SYMBOL_QUOTE

						' Requires at least one argument to be passed in.
						Self.throwExceptionIfNoArgs(args)

						Self._result = args.car()

					Case LispMax_Atom.SYMBOL_DEFINE

						Local sym:LispMax_Atom
						Local val:LispMax_atom

						If args.isNil() Or args.cdr().isNil() Then
							Throw New Lispmax_ArgumentException
						End If

						sym = args.value_pair.atom[LISPMAX_CAR]

						If sym.isAtomType(LispMax_Atom.ATOM_TYPE_PAIR) Then

							' Function
							val = LispMax.makeClosure(Self._environment, sym.cdr(), args.cdr())
							sym = sym.value_pair.atom[LISPMAX_CAR]

							If sym.isAtomType(LispMax_Atom.ATOM_TYPE_SYMBOL) = False Then
								Throw New Lispmax_UnexpectedTypeException
							End If

							Self._environment.set(sym, val)

							Self._result = sym

						ElseIf sym.isAtomType(LispMax_Atom.ATOM_TYPE_SYMBOL) Then

							' Enter the new stack
							Self._stack       = LispMax_StackFrame.Create(Self._stack, Self._environment, LispMax_Nil)
							Self._stack._op   = op
							Self._stack._args = sym

							Self._expression = args.cdar()

							Return False

						Else
							Throw New Lispmax_UnexpectedTypeException
						End If

					Case LispMax_Atom.SYMBOL_DEFUN

					' (defun SYMBOL (ARGS MAYBE) DOCSTRING-MAYBE PROGN STUFF)

						Local functionName:LispMax_Atom
						Local functionArgs:LispMax_Atom
						Local docString:LispMax_Atom
						Local functionBody:LispMax_Atom

						If args.isNil() Or args.cdr().isNil() Or args.cdar().isNil() Then
							Throw New Lispmax_ArgumentException
						EndIf

						functionName = args.car()
						functionArgs = args.cdar()
						docString    = args.cddar()

						' Check args were set
						If functionArgs.atom_type = 0 Then
							functionArgs = LispMax_Nil
						End If

						' Check doc string is actually present. If not, make sure they body gets set correctly
						If docString.isAtomType(LispMax_Atom.ATOM_TYPE_STRING) And Not(args.cdddr().car().isNil()) Then
							' Doc string is present
							functionBody = args.cdddr()
						Else
							' No docstring. Nill it and expand the body
							docString = LispMax_Nil
							functionBody = args.cddr()
						EndIf

						' Create a closure for the new function and add it to the environment
						Local newFunction:LispMax_Atom = LispMax.makeClosure(Self._environment, functionArgs, functionBody)
						Self._environment.set(functionName, newFunction)

						' Return the function name
						Self._result = functionName

					Case LispMax_Atom.SYMBOL_PROGN

						' Empty progn evaluates to nil
						If args.isNil() Then
							Self._result = LispMax_Nil
						Else

							Self._stack		  = LispMax_StackFrame.Create(Self._stack, Self._environment, Self.makeNil())
							Self._stack._op   = Self.makeClosure(Self._environment, Self.makeNil(), args.car())
							Self._stack._body = Self.makeClosure(Self._environment, Self.makeNil(), args)

						End If

					Case LispMax_Atom.SYMBOL_LAMBDA

						If args.isNil() Or args.cdr().isNil() Then
							Throw New Lispmax_ArgumentException
						End If

						Self._result = LispMax.makeClosure(Self._environment, args.car(), args.cdr())

					Case LispMax_Atom.SYMBOL_IF

						' Check there is a condition, a true expression and a false expression
						If args.isNil() Or args.cdr().isNil() Or args.cddr().isNil() Or Not(args.cddr().cdr().isNil()) Then
							Throw New Lispmax_ArgumentException
						End If

						' Push onto the stack
						Self._stack = LispMax_StackFrame.Create(Self._stack, Self._environment, args.cdr())
						Self._stack.setOperator(op)
						Self._expression = args.car()

						Return False

					Case LispMax_Atom.SYMBOL_WHEN, ..
						 LispMax_Atom.SYMBOL_UNLESS

						' Check there is a condition and a body
						If args.isNil() Or args.cdr().isNil() Then
							Throw New Lispmax_ArgumentException
						End If

						' Push onto the stack
						Self._stack = LispMax_StackFrame.Create(Self._stack, Self._environment, args.cdr())
						Self._stack.setOperator(op)
						Self._expression = args.car()

						Return False

					Case LispMax_Atom.SYMBOL_DEFMACRO

						Local name:LispMax_Atom
						Local macro:LispMax_atom

						If args.isNil() Or args.cdr().isNil() Then
							Throw New Lispmax_ArgumentException
						End If

						If Not(args.car().isAtomType(LispMax_Atom.ATOM_TYPE_PAIR)) Then
							Throw New Lispmax_SyntaxErrorException
						End If

						name = args.caar()

						If name.atom_type <> LispMax_Atom.ATOM_TYPE_SYMBOL Then
							Throw New Lispmax_UnexpectedTypeException
						End If

						macro = LispMax.makeClosure(Self._environment, args.cadr(), args.cdr())
						macro.atom_type = LispMax_Atom.ATOM_TYPE_MACRO

						Self._environment.set(name, macro)

						Self._result = name

					Case LispMax_Atom.SYMBOL_APPLY

						If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
							Throw New Lispmax_ArgumentException
						EndIf

						Self._stack = LispMax_StackFrame.Create(Self._stack, Self._environment, args.cdr())
						Self._stack._op = op
						Self._expression = args.car()

						Return False

					Case LispMax_Atom.SYMBOL_SUSPEND

						Self.stopProcess()
						Self.moveToNextExpression()

						Return False

					Default
						Self._stack      = LispMax_StackFrame.Create(Self._stack, Self._environment, args)
						Self._expression = op

						Return False

				End Select

			ElseIf op.atom_type = LispMax_Atom.ATOM_TYPE_BUILTIN Then
				Self._result = op.value_builtin.call(Self, args)
			Else

				' Push to stack
				Self._stack = LispMax_StackFrame.Create(Self._stack, Self._environment, args)
				Self._expression = op
				Return False

			EndIf

		EndIf

		' If stack is empty, we're done.
		If Self._stack = Null Then
			Return True
		End If

		' Otherwise store the result and move to next expression.
		Self.doReturn()

	End Method


	' ----------------------------------------------------------------------
	' -- Library loading
	' ----------------------------------------------------------------------

	''' <summary>
	''' Loads, parses and evaluates a file containing lisp expressions.
	''' </summary>
	Method loadFile(environment:LispMax_Environment, url:Object)

		' Open the url, read all contents and close
		Local content:String = Self.readAll(url)

		' Initialize the lexer and parse the first expression
		Local expression:LispMax_Atom = Self.parseExpression(content)

		' Read an evaluate all remaining expressions
		Repeat
			Self.evaluateExpression(expression, environment)
			expression = Self.readExpression()
		Until expression = Null

	End Method

	''' <summary>Read the entire contents of a file and return as a string.</summary>
	Method readAll:String(url:Object)

		' Create a bank.
		Local bank:TBank = LoadBank(url)
		Local size:Int   = BankSize(bank)

		' Ensure bank finishes with a 0 byte.
		' This prevents the string from having junk bytes at the end.
		ResizeBank(bank, size + 1)
		PokeByte(bank, size - 1, 0)

		' Get buffer contents.
		Local content:String  = ""
		Local buffer:Byte Ptr = LockBank(bank)

		' Convert to a string.
		content = String.FromCString(buffer)

		' Cleanup.
		UnlockBank(bank)
		bank = null

		Return content

	End Method


	' ----------------------------------------------------------------------
	' -- Parsing
	' ----------------------------------------------------------------------

	''' <summary>
	''' Initialises the lexer with the contents of a string and returns the
	''' first expression as an atom.
	''' </summary>
	Method parseExpression:LispMax_Atom(source:String)

		' Check source is null terminated
		If source.endswith("~0") = False Then source :+ "~0"

		' Create the lexer
		Self._lexer = LispMax_Lexer.Create(source)

		' Read the first expression
		Return Self.readExpression()

	End Method

	''' <summary>
	''' Reads an expression from the current lexer and returns the read result
	''' as an atom.
	''' </summary>
	''' <param name="token">
	''' Optional current token to read from. If no token is passed in, one
	''' will be read from the lexer.
	''' </param>
	''' <return>The read expression in the form of a lisp atom.</return>
	Method readExpression:LispMax_Atom(token:LispMax_Token = null)

		' Get the first token
		If token = Null Then token = Self._lexer.readToken()
		If token = Null Then Return Null

		Select token.tokenType

			' Start of a new list.
			Case LispMax_Token.TOKEN_OPEN_PAREN
				Return Self.readList()

			' Expression terminated too early - error
			Case LispMax_Token.TOKEN_CLOSE_PAREN
				' TODO: Display some information about the expression here.
				Throw New Lispmax_SyntaxErrorException

			' Quoted expression
			Case LispMax_Token.TOKEN_QUOTE
				Return Self.readPrefixedExpression("QUOTE")

			Case LispMax_Token.TOKEN_QUASIQUOTE
				Return Self.readPrefixedExpression("QUASIQUOTE")

			Case LispMax_Token.TOKEN_UNQUOTE
				Return Self.readPrefixedExpression("UNQUOTE")

			Case LispMax_Token.TOKEN_UNQUOTE_SPLICING
				Return Self.readPrefixedExpression("UNQUOTE-SPLICING")

			' Anything else is just a simple atom
			Default
				Return Self.parseSimple(token)

		End Select

	End Method

	Method readPrefixedExpression:Lispmax_Atom(prefix:String)

		' Create a new cons cell that starts with the prefix
		Local result:LispMax_Atom = Self.cons(Self.makeSymbol(prefix), Self.cons(Self.makeNil(), Self.makeNil()))

		' Set quotes to a special char
		If prefix = "QUOTE" Then
			result.car().special_symbol = LispMax_Atom.SYMBOL_QUOTE
		EndIf

		' Read the expression into the second cell
		result.cdr().car(Self.readExpression())

		' Return the result
		Return result

	End Method

	''
	' Reads a list from the currently active parser. Adds itself to the
	' currently active expression.
	Method readList:LispMax_Atom()

		' Create an atom
		Local expression:LispMax_Atom = LispMax.makeAtom()
		Local endOfList:LispMax_Atom  = Null

		' Keep reading until we reach the end of the list a ')' symbol
		Repeat

			' Get the current token
			Local token:LispMax_Token = Self._lexer.readToken()

			' If we reached the end of the list, return it
			If token.tokenType = LispMax_Token.TOKEN_CLOSE_PAREN Then
				Return expression
			EndIf

			' Check for improper lists
			if token.contents = "." then

				If expression.isNil() Then
					Throw New Lispmax_SyntaxErrorException
				EndIf

				' Read the next expression to create the pair
				Local nextExpression:LispMax_Atom = Self.readExpression()
				expression.cdr(nextExpression)

				' Read the next token
				Local nextToken:Lispmax_Token = Self._lexer.readToken()
				If nextToken.tokenType <> LispMax_Token.TOKEN_CLOSE_PAREN Then
					Throw New Lispmax_SyntaxErrorException
				EndIf

				Return expression

			endif

			Local nextExpression:LispMax_Atom = self.readExpression(token)

			' If it's nil, which it shouldn't be///
			If nextExpression.isNil() Then
				expression = LispMax.cons(nextExpression, LispMax.makeNil())
			EndIf

			' Initial value goes at the start of the list
			If expression.car() = null then
				expression = LispMax.cons(nextExpression, LispMax.makeNil())
			Else

				' Push to end
				If endOfList = Null Then
					endOfList = expression.cdr(LispMax.cons(nextExpression, LispMax.makeNil()))
				Else
					endOfList = endOfList.cdr(LispMax.cons(nextExpression, LispMax.makeNil()))
				EndIf

			End If

		Forever

	End Method

	''' <summary>Parses a simple expression and returns the appropriate atom.</summary>
	Method parseSimple:LispMax_Atom(token:LispMax_Token)

		Select token.tokenType

			Case LispMax_Token.TOKEN_NUMBER
				Return Self.makeNumber(Long(token.contents))

			Case LispMax_Token.TOKEN_SYMBOL

				If token.contents = "NIL" Then
					Return LispMax.makeNil()
				Else

					Local symbol:LispMax_Atom = Self.makeSymbol(token.contents)

					Select symbol.value_symbol

						Case "'"		; symbol.special_symbol = LispMax_Atom.SYMBOL_QUOTE
						Case "QUOTE"	; symbol.special_symbol = LispMax_Atom.SYMBOL_QUOTE
						Case "DEFMACRO" ; symbol.special_symbol = LispMax_Atom.SYMBOL_DEFMACRO
						Case "DEFINE"   ; symbol.special_symbol = LispMax_Atom.SYMBOL_DEFINE
						Case "DEFUN"    ; symbol.special_symbol = LispMax_Atom.SYMBOL_DEFUN
						Case "PROGN"	; symbol.special_symbol = LispMax_Atom.SYMBOL_PROGN
						Case "SETQ"		; symbol.special_symbol = LispMax_Atom.SYMBOL_SETQ
						Case "LAMBDA"   ; symbol.special_symbol = LispMax_Atom.SYMBOL_LAMBDA
						Case "IF"		; symbol.special_symbol = LispMax_Atom.SYMBOL_IF
						Case "SUSPEND"  ; symbol.special_symbol = LispMax_Atom.SYMBOL_SUSPEND
						Case "WHEN"		; symbol.special_symbol = LispMax_Atom.SYMBOL_WHEN
						Case "UNLESS"	; symbol.special_symbol = LispMax_Atom.SYMBOL_UNLESS

					End Select

					Return symbol

				EndIf

			Case LispMax_Token.TOKEN_LITERAL
				Return Self.makeString(token.contents)

			Default
				Throw Lispmax_SyntaxErrorException.Create("Unexpected/Unknown token during parse: " + token.contents)

		End Select

	End Method


	' ----------------------------------------------------------------------
	' -- Environments
	' ----------------------------------------------------------------------

	''
	' Create a new environment and assign it a parent
	Method createEnvironment:LispMax_Atom(parent:LispMax_Atom)
		Return LispMax.cons(parent, LispMax.makeNil())
	End Method

	Method getEnvironmentValue:LispMax_Atom(env:LispMax_Environment, symbol:LispMax_Atom)
		Return env.get(symbol)
	End Method

	Method setEnvironmentValue(env:Lispmax_Environment, symbol:LispMax_Atom, value:LispMax_Atom)
		env.set(symbol, value)
	End Method

	''' <summary>Dump the contents of the environment to output.</summary>
	Method dumpEnvironment:String(env:Lispmax_Atom)
		Self.printExpression(env)
	End Method

	' Create and return a symbol atom that is not part of a LispMax instance.
	' TODO: Can this be removed?
	Function getGlobalSymbol:LispMax_Atom(name:String)
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_type    = LispMax_Atom.ATOM_TYPE_SYMBOL
		a.value_symbol = name.ToUpper()

		Return a
	End Function


	' ----------------------------------------------------------------------
	' -- Creating Atoms
	' ----------------------------------------------------------------------

	' TODO: Update this to allow object pooling
	Function makeAtom:LispMax_Atom()
		Return New LispMax_Atom
	End Function

	' TODO: Replace this with `makeInteger`
	' TODO: Would be nice to treat all numbers the same (i.e. support floats and adding them to integers).
	Method makeNumber:Lispmax_Atom(x:Long)
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_type   = LispMax_Atom.ATOM_TYPE_INTEGER
		a.value_number = x

		Return a
	End Method

	''
	' Creates a new cons atom
	Function cons:LispMax_Atom(car_val:LispMax_Atom, cdr_val:LispMax_Atom)
		Local p:LispMax_atom = LispMax.makeAtom()

		p.atom_type          = LispMax_Atom.ATOM_TYPE_PAIR
		p.value_pair.atom[0] = car_val
		p.value_pair.atom[1] = cdr_val

		Return p
	End Function

	''
	' Create an integer atom
	Function MakeInt:LispMax_Atom(x:Long)
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_Type    = LispMax_Atom.ATOM_TYPE_INTEGER
		a.value_number = x

		Return a
	End Function

	Function MakeString:LispMax_Atom(x:String)
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_type    = LispMax_Atom.ATOM_TYPE_STRING
		a.value_symbol = x

		Return a
	End Function

	Function MakeBuiltin:LispMax_Atom(fn:LispMax_Atom(caller:LispMax, args:LispMax_Atom))
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_type     = LispMax_Atom.ATOM_TYPE_BUILTIN
		a.value_builtin = Lispmax_Builtin.Create(fn)

		Return a
	End Function

	Function MakeCallable:LispMax_Atom(callback:Lispmax_Callable)
		Local a:LispMax_Atom = LispMax.makeAtom()

		a.atom_type     = LispMax_Atom.ATOM_TYPE_BUILTIN
		a.value_builtin = callback

		Return a
	End Function

	''' <summary>
	''' Create a symbol atom called NAME and add it to the symbol table.
	'''
	''' Returns The existing symbol instance if it is already present in the
	''' symbol table.
	''' </summary>
	Method makeSymbol:LispMax_Atom(name:String)

		' Clean the name.
		name = name.toUpper()

		' Check the symbol table first.
		Local symbol:LispMax_Atom = Self.symbolTable.get(name)

		If symbol = Null Then
			' Not found - create a new one.
			symbol              = LispMax.makeAtom()
			symbol.atom_type    = LispMax_Atom.ATOM_TYPE_SYMBOL
			symbol.value_symbol = name

			' Add to the symbol table.
			Self.symbolTable.set(name, symbol)
		End If

		Return symbol

	End Method

	Function makeNil:LispMax_Atom()
		Return LispMax_Nil
	End Function

	Method makeBool:LispMax_Atom(bool:Byte)
		If bool Then
			Return Self.makeSymbol("T")
		Else
			Return Self.makeNil()
		End If
	End Method

	Function copyList:LispMax_Atom(list:LispMax_Atom)
		Local a:LispMax_Atom
		Local p:LispMax_Atom

		' No need to copy anything if list is nil
		If list.isNil() Then
			Return LispMax.makeNil()
		End If

		a = LispMax.cons(list.car(), LispMax.makeNil())
		p = a

		list = list.cdr()

		While list.isNil() = False
			p.cdr(LispMax.cons(list.car(), LispMax.makeNil()))
			p = p.cdr()
			list = list.cdr()
		WEnd

		Return a
	End Function

	''' <summary>
	''' Creates a new closure.
	'''
	''' Closure is as follows (args . body).
	''' </summary>
	Function makeClosure:LispMax_Atom(env:LispMax_Environment, args:LispMax_Atom, body:LispMax_Atom)

		' Check there is a function body
		If Not body.isList() Then
			Print "Body too short"
			Throw New Lispmax_SyntaxErrorException
		EndIf

		' Check argument names are all symbols
		Local p:Lispmax_Atom = args

		While Not p.isNil()

			If p.isAtomType(LispMax_Atom.ATOM_TYPE_SYMBOL) Then
				Exit
			ElseIf p.atom_type = LispMax_Atom.ATOM_TYPE_PAIR Then
				Exit
			ElseIf Not(p.isAtomType(LispMax_Atom.ATOM_TYPE_PAIR)) Or Not(p.car().isAtomType(LispMax_Atom.ATOM_TYPE_SYMBOL)) Then
				Print "ATOM TYPE: " + p.atom_type
				Throw Lispmax_UnexpectedTypeException.Create(args.ToString(), LispMax_Atom.ATOM_TYPE_PAIR, p.atom_type)
			End If

			p = p.cdr()

		Wend

		' Create the new closure
		Local result:LispMax_Atom = LispMax.cons(args, body)

		result._environment = env
		result.atom_type = LispMax_Atom.ATOM_TYPE_CLOSURE

		Return result

	End Function

	''' <summary>
	''' Applies a lisp function to a list of arguments and returns the result.
	''' </summary>
	Method apply:LispMax_Atom(fn:LispMax_Atom, args:LispMax_Atom)

		' Call built in functions
		If fn.isAtomType(LispMax_Atom.ATOM_TYPE_BUILTIN) Then
			Return fn.value_builtin.call(Self, args)
		ElseIf fn.isAtomType(LispMax_Atom.ATOM_TYPE_CLOSURE) = False Then

'			Local pointer:Lispmax_Atom = self._environment.get(fn)
'			if pointer.isAtomType(Lispmax_Atom.ATOM_TYPE_BUILTIN) then
'				Return pointer.value_builtin.call(Self, args)
'			endif

			throw Lispmax_UnexpectedTypeException.create("APPLY", Lispmax_Atom.ATOM_TYPE_CLOSURE, fn.atom_type)
		End If

		' Not a built-in, so will be a lambda expression
		Local env:LispMax_Environment = Lispmax_Environment.Create(fn._environment)
		Local argNames:LispMax_Atom   = fn.car()
		Local body:LispMax_Atom		  = fn.cdr()

		Print "Starting ---"
		Print "argNames : " + Self.expressionToString(argNames)
		Print "args     : " + Self.expressionToString(args)

		Print "Looping ---"

		' Bind the arguments
		Repeat

			' Check for variadic arguments
			If argNames.isAtomType(LispMax_Atom.ATOM_TYPE_SYMBOL) Then
				env.set(argNames, args)
				args = Self.makeNil()
				Exit
			End If

			' If not enough variables were passed in, throw an error
			If args.isNil() Then
				Print "argNames : " + Self.expressionToString(argNames)
				Print "args     : " + Self.expressionToString(args)
				Throw New Lispmax_ArgumentException
			End If

			' Add the variable to the function's environment
			env.set(argNames.car(), args.car())

			' Move to the next argument name/value
			argNames = argNames.cdr()
			args	 = args.cdr()

		Until argNames.isNil()

		' If not all arguments were bound, throw an error
		If args.isNil() = False Then
			Print "argNames : " + Self.expressionToString(argNames)
			Print "args     : " + Self.expressionToString(args)
			Throw New Lispmax_ArgumentException
		End If

		' Evaluate the body
		Local result:LispMax_Atom
		While body.isNil() = False
			result = Self.evaluateExpression(body.car(), env)
			body = body.cdr()
		WEnd

		Return result

	End Method



	' ----------------------------------------------------------------------
	' -- Stack utilities
	' ----------------------------------------------------------------------

	''' <summary>Store an evaluated arguments on the stack.</summary>
	Method storeArg:LispMax_Atom(result:Lispmax_Atom)
		Local args:LispMax_Atom = Self._stack._args
		Self._stack._args       = Self.cons(result, args)

		Return args
	End Method

	Method listReverse:Lispmax_Atom(list:LispMax_Atom)
		' Don't bother reversing empty lists.
		If LispMax_Atom.ATOM_TYPE_NIL = list.atom_Type Then Return LispMax_Nil

		' Reverse the list.
		Local tail:LispMax_Atom = LispMax_Nil
		Local p:LispMax_Atom    = LispMax_Nil

		Repeat
			' TODO: Below is ever so slightly faster, but not as readable.
'			p = list.value_pair.atom[LISPMAX_CDR]
'			list.value_pair.atom[LISPMAX_CDR] = tail

			p = list.cdr()
			list.cdr(tail)

			tail = list
			list = p
		Until LispMax_Atom.ATOM_TYPE_NIL = list.atom_type

		Return tail

	End Method

	''' <summary>
	''' Get key names from a list of arguments and their defaults.
	''' </summary>
	Method getExpressionKeys:LispMax_Atom(list:LispMax_Atom)

		Local keys:LispMax_Atom = Self.makeNil()

		While list.isNil() = False

			' Get the name
			Local key:LispMax_Atom = list.car()

			' All done if reached "&REST"
			If key.value_symbol = "&REST" Then Return keys

			If key.isPairAtom() Then
				key = Self.cons(key.car(), key.cdar())
			Else
				key = Self.cons(key, Self.makeNil())
			End If

			' Push to the list of keys
			keys = Self.append(keys, key)

			list = list.cdr()

		Wend

		Return keys

	End Method

	Method listContainsValue:Byte(list:LispMax_Atom, value:String)

		While list.isNil() = False
			If list.caar().value_symbol = value Then Return True
			list = list.cdr()
		Wend

		Return False

	End Method

	' [todo] - This is very very slow
	Method append:LispMax_Atom(list:LispMax_Atom, value:LispMax_Atom)
		Return Self.listReverse(Self.cons(value, Self.listReverse(list)))
	End Method


	' ----------------------------------------------------------------------
	' -- Debugging Helpers
	' ----------------------------------------------------------------------

	''' <summary>Prints an expression to the console.</summary>
	''' <param name="atom">The expression to print.</param>
	Method printExpression(atom:LispMax_Atom)
		Print Self.expressionToString(atom)
	End Method

	''' <summary>Converts an expression to a string and returns it.</summary>
	''' <param name="atom">The expression to convert.</param>
	Method expressionToString:String(atom:LispMax_Atom)

		Local output:String = ""

		Select atom.atom_type

			Case LispMax_Atom.ATOM_TYPE_NIL
				output :+ "NIL"

			Case LispMax_Atom.ATOM_TYPE_SYMBOL
				output :+ atom.value_symbol

			Case LispMax_Atom.ATOM_TYPE_INTEGER
				output :+ atom.value_number

			Case LispMax_Atom.ATOM_TYPE_STRING
				output :+ "~q" + atom.value_symbol + "~q"

			Case LispMax_Atom.ATOM_TYPE_BUILTIN
				output :+ "#<BUILTIN:" + atom.value_builtin.ToString() + ">"

			Case LispMax_Atom.ATOM_TYPE_PAIR
				output :+ "("
				output :+ Self.expressionToString(atom.car())
				atom = atom.cdr()

				While (Not(atom.isNil()))
					If atom.atom_Type = LispMax_Atom.ATOM_TYPE_PAIR Then
						output :+ " "
						output :+ Self.expressionToString(atom.car())
						atom = atom.cdr()
					Else
						output :+ " . "
						output :+ Self.expressionToString(atom)
						Exit
					EndIf
				Wend

				output :+ ")"

			Case LispMax_Atom.ATOM_TYPE_CLOSURE
				output:+ Self.expressionToString(atom.car()) + " " + Self.expressionToString(atom.cdr())

			Default
				output :+ ("ATOM: " + atom.atom_type)

		End Select

		Return output

	End Method

	Method throwExceptionIfNoArgs(args:LispMax_Atom)
		If args.isNil() Or Not(args.cdr().isNil()) Then
			Throw New Lispmax_ArgumentException
		End If
	End Method


	' ----------------------------------------------------------------------
	' -- Engine functions
	' ----------------------------------------------------------------------

	''' <summary>Reset internals and run garbage collection.</summary>
	Method reset()
		Self._result     = Null
		Self._stack      = Null
		Self._expression = Null
		GCCollect()
	End Method


	' ----------------------------------------------------------------------
	' -- Atom Helpers
	' ----------------------------------------------------------------------

	Function GetAtomTypeAsString:String(atomType:Int)

		Select atomType
			Case LispMax_Atom.ATOM_TYPE_INTEGER ; Return "INTEGER"
			Case LispMax_Atom.ATOM_TYPE_SYMBOL	; Return "SYMBOL"
			Case LispMax_Atom.ATOM_TYPE_NIL		; Return "NIL"
			Case LispMax_Atom.ATOM_TYPE_PAIR	; Return "PAIR"
			Case LispMax_Atom.ATOM_TYPE_BUILTIN	; Return "BUILTIN"
			Case LispMax_Atom.ATOM_TYPE_CLOSURE	; Return "CLOSURE"
			Case LispMax_Atom.ATOM_TYPE_MACRO	; Return "MACRO"
			Case LispMax_Atom.ATOM_TYPE_STRING	; Return "STRING"
			Default								; Return "Unknown"
		End Select

	End Function


	' ----------------------------------------------------------------------
	' -- Construction
	' ----------------------------------------------------------------------

	''' <summary>
	''' Create a new environment, register all builtin functions and load
	''' lisp library functions and macros from the "site-lisp/library.lisp"
	''' file (included via incbin).
	''' </summary>
	Method initializeEnvironment()

		' Create a new environment.
		Self._environment = LispMax_Environment.Create()

		' Register builtin functions.
		Self._environment.set(Self.makeSymbol("CAR"), Self.makeBuiltin(LispMax_Builtin_Car))
		Self._environment.set(Self.makeSymbol("CDR"), Self.makeBuiltin(LispMax_Builtin_Cdr))
		Self._environment.set(Self.makeSymbol("CONS"), Self.makeBuiltin(LispMax_Builtin_Cons))
		Self._environment.set(Self.makeSymbol("NTH"), Self.makeBuiltin(LispMax_Builtin_Nth))
		Self._environment.set(Self.makeSymbol("+"), Self.makeBuiltin(LispMax_Builtin_Add))
		Self._environment.set(Self.makeSymbol("-"), Self.makeBuiltin(LispMax_Builtin_Subtract))
		Self._environment.set(Self.makeSymbol("*"), Self.makeBuiltin(LispMax_Builtin_Multiply))
		Self._environment.set(Self.makeSymbol("/"), Self.makeBuiltin(LispMax_Builtin_Divide))
		Self._environment.set(Self.makeSymbol("MOD"), Self.makeBuiltin(LispMax_Builtin_Mod))
		Self._environment.set(Self.makeSymbol("="), Self.makeBuiltin(LispMax_Builtin_NumEQ))
		Self._environment.set(Self.makeSymbol("<"), Self.makeBuiltin(LispMax_Builtin_NumLT))
		Self._environment.set(Self.makeSymbol(">"), Self.makeBuiltin(LispMax_Builtin_NumGT))
		Self._environment.set(Self.makeSymbol("EQ?"), Self.makeBuiltin(LispMax_Builtin_EQ))
		Self._environment.set(Self.makeSymbol("NIL?"), Self.makeBuiltin(LispMax_Builtin_IsNil))
		Self._environment.set(Self.makeSymbol("PAIR?"), Self.makeBuiltin(LispMax_Builtin_Pair))
		Self._environment.set(Self.makeSymbol("APPLY"), Self.makeBuiltin(LispMax_Builtin_Apply))
		Self._environment.set(Self.makeSymbol("LISTP"), Self.makeBuiltin(LispMax_Builtin_ListP))
		Self._environment.set(Self.makeSymbol("FAST-FOLDR"), Self.makeBuiltin(LispMax_Builtin_FoldR))
		Self._environment.set(Self.makeSymbol("LIST"), Self.makeBuiltin(LispMax_Builtin_List))
		Self._environment.set(Self.makeSymbol("OR"), Self.makeBuiltin(LispMax_Builtin_Or))

		' String functions.
		Self._environment.set(Self.makeSymbol("STRING-UPCASE"), Self.makeBuiltin(LispMax_Builtin_StringUpcase))
		Self._environment.set(Self.makeSymbol("STRING-DOWNCASE"), Self.makeBuiltin(LispMax_Builtin_StringDowncase))

		' LispMax stuff.
		Self._environment.set(Self.makeSymbol("RAND"), Self.makeBuiltin(LispMax_Builtin_Rand))
		Self._environment.set(Self.makeSymbol("MILLISECS"), Self.makeBuiltin(LispMax_Builtin_Millisecs))
		Self._environment.set(Self.makeSymbol("PARSE-INTEGER"), Self.makeBuiltin(LispMax_Builtin_ParseInteger))

		' Standard IO.
		Self._environment.set(Self.makeSymbol("DEBUGLOG"), Self.makeBuiltin(LispMax_Builtin_Debuglog))
		Self._environment.set(Self.makeSymbol("PRINT"), Self.makeBuiltin(LispMax_Builtin_Print))
		Self._environment.set(Self.makeSymbol("PRINT-EXPRESSION"), Self.makeBuiltin(LispMax_Builtin_PrintExpression))

		' Register common symbols.
		Self._environment.set(Self.makeSymbol("T"), Self.makeSymbol("T"))
		Self._environment.set(Self.makeSymbol("NIL"), Self.makeNil())
		Self._environment.set(Self.makeSymbol("&REST"), Self.makeSymbol("&REST"))
		Self._environment.set(Self.makeSymbol("&OPTIONAL"), Self.makeSymbol("&OPTIONAL"))
		Self._environment.set(Self.makeSymbol("&KEY"), Self.makeSymbol("&KEY"))

		' Load lisp functions and macros from library.
		Self.loadFile(Self._environment, "incbin::site-lisp/library.lisp")

	End Method


	' ----------------------------------------------------------------------
	' -- Construction
	' ----------------------------------------------------------------------

	Method New()
		If LispMax_Nil = Null Then
			LispMax_Nil             = LispMax.makeAtom()
			LispMax_Nil.atom_type   = LispMax_Atom.ATOM_TYPE_NIL
			Lispmax_Nil.value_symbol = "NIL"
		End If

		self.symbolTable = New LispMax_AtomMap
	End Method

End Type
