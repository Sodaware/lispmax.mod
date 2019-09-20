' ------------------------------------------------------------------------------
' -- lispmax_builtin.bmx
' --
' -- Builtin functions for LispMax. These are functions that need to be fast, so
' -- they're built directly into the engine.
' --
' -- A lot of these functions access internals directly to keep them as fast as
' -- possible. They're quite ugly and brittle.
' --
' -- This file is part of sodaware.mod (https://www.sodaware.net/sodaware.mod/)
' -- Copyright (c) 2009-2019 Phil Newton
' --
' -- See LICENSE for full license information.
' ------------------------------------------------------------------------------


' ----------------------------------------------------------------------
' -- Assertion Helpers
' ----------------------------------------------------------------------

Function LispMax_Assert_HasSingleArg(args:LispMax_Atom)
	If args.atom_type = LispMax_Atom.ATOM_TYPE_NIL Then Throw New Lispmax_ArgumentException
	if args.value_pair.atom[LISPMAX_CDR].atom_type <> LispMax_Atom.ATOM_TYPE_NIL Then Throw New Lispmax_ArgumentException
End Function


' ----------------------------------------------------------------------
' -- Atom Functions
' ----------------------------------------------------------------------

Function LispMax_Builtin_Car:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' Check that ARGS contains only one element.
	LispMax_Assert_HasSingleArg(args)

	' If a nil or empty element was passed in, return NIL.
	If args.value_pair.atom[LISPMAX_CAR].atom_type = LispMax_Atom.ATOM_TYPE_NIL Then Return LispMax_Nil
	If args.value_pair.atom[LISPMAX_CAR].atom_type = 0 Then Return LispMax_Nil

	' Throw an error if the element passed in is NOT a pair.
	If args.value_pair.atom[LISPMAX_CAR].atom_type <> LispMax_Atom.ATOM_TYPE_PAIR Then
		throw Lispmax_UnexpectedTypeException.create("CAR", Lispmax_Atom.ATOM_TYPE_PAIR, args.car().atom_type)
	End If

	Return args.value_pair.atom[LISPMAX_CAR].value_pair.atom[LISPMAX_CAR]

End Function

Function LispMax_Builtin_Cdr:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' Check that ARGS contains only one element.
	LispMax_Assert_HasSingleArg(args)

	' If a nil or invalid element was passed in, return NIL
	If args.value_pair.atom[LISPMAX_CAR].atom_type = LispMax_Atom.ATOM_TYPE_NIL Then Return LispMax_Nil
	If args.value_pair.atom[LISPMAX_CAR].atom_type = 0 Then Return LispMax_Nil

	' Throw an error if the element passed in is NOT a pair.
	If args.value_pair.atom[LISPMAX_CAR].atom_type <> LispMax_Atom.ATOM_TYPE_PAIR Then
		throw Lispmax_UnexpectedTypeException.create("CDR", Lispmax_Atom.ATOM_TYPE_PAIR, args.car().atom_type)
	End If

	Return args.value_pair.atom[LISPMAX_CAR].value_pair.atom[LISPMAX_CDR]

End Function

Function LispMax_Builtin_Cons:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' Check that ARGS contains only TWO elements.
	If args.atom_type = LispMax_Atom.ATOM_TYPE_NIL Then Throw New Lispmax_ArgumentException
	if args.value_pair.atom[LISPMAX_CDR].atom_type = LispMax_Atom.ATOM_TYPE_NIL Then Throw New Lispmax_ArgumentException
	if args.value_pair.atom[LISPMAX_CDR].value_pair.atom[LISPMAX_CDR].atom_type <> LispMax_Atom.ATOM_TYPE_NIL Then Throw New Lispmax_ArgumentException

	Return lisp.cons(args.value_pair.atom[LISPMAX_CAR], args.value_pair.atom[LISPMAX_CDR].value_pair.atom[LISPMAX_CAR])

End Function

' [todo] - Need some error checking here!

Function LispMax_Builtin_Nth:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local value:LispMax_Atom = args.cdar()
	For Local i:Int = 1 To args.car().value_number
		value = value.cdr()
	Next

	Return value.car()

End Function


' ----------------------------------------------------------------------
' -- Type Functions
' ----------------------------------------------------------------------

Function LispMax_Builtin_ParseInteger:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	If args.car().isNil() Then
		Return LispMax_Nil
	EndIf

	Return lisp.makeNumber(Long(args.car().value_symbol))

End Function


' ----------------------------------------------------------------------
' -- Arithmetic
' ----------------------------------------------------------------------

Function LispMax_Builtin_Add:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		lisp.printExpression(args)
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom = args.car()
	Local b:Lispmax_Atom = args.cdar()

	' TODO: These should show a little more information. Want to know the expression and parameter.
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw Lispmax_UnexpectedTypeException.Create("+", LispMax_Atom.ATOM_TYPE_INTEGER, a.atom_type)
	End If

	If b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw Lispmax_UnexpectedTypeException.Create("+", LispMax_Atom.ATOM_TYPE_INTEGER, b.atom_type)
	End If

	Return LispMax.MakeInt(a.value_number + b.value_number)

End Function

Function LispMax_Builtin_Subtract:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return LispMax.MakeInt(a.value_number - b.value_number)

End Function

Function LispMax_Builtin_Multiply:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return LispMax.MakeInt(a.value_number * b.value_number)

End Function

Function LispMax_Builtin_Divide:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return LispMax.MakeInt(a.value_number / b.value_number)

End Function

Function LispMax_Builtin_Mod:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return LispMax.MakeInt(a.value_number mod b.value_number)

End Function

Function LispMax_Builtin_NumEQ:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Local result:LispMax_Atom = LispMax.makeNil()
	If a.value_number = b.value_number Then result = lisp.makeSymbol("T")

	Return result

End Function

Function LispMax_Builtin_NumLT:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Local result:LispMax_Atom = LispMax.makeNil()
	If a.value_number < b.value_number Then result = lisp.makeSymbol("T")

	Return result

End Function

Function LispMax_Builtin_NumGT:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Local result:LispMax_Atom = LispMax.makeNil()
	If a.value_number > b.value_number Then result = lisp.makeSymbol("T")

	Return result

End Function


' ----------------------------------------------------------------------
' -- Misc Maths
' ----------------------------------------------------------------------

Function LispMax_Builtin_Rand:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' Check there's a lower / top
	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()

	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return lisp.makeNumber(Rand(a.value_number, b.value_number))

End Function

Function LispMax_Builtin_Millisecs:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	Return lisp.makeNumber(MilliSecs())
End Function


' ----------------------------------------------------------------------
' -- Logic
' ----------------------------------------------------------------------

' TODO: Is this ever called?
Function LispMax_Builtin_And:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		' TODO: Include an error message
		Throw New Lispmax_ArgumentException
	EndIf

	' If either is nil, return nil.
	Local a:Lispmax_Atom = args.car()
	Local b:Lispmax_Atom = args.cdar()

	Return lisp.makeBool(False = a.isNil() And False = b.isNil())

End Function

Function LispMax_Builtin_Or:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	While args.isNil() = False

		If Not args.car().isNil() Return args.car()

		args = args.cdr()
	Wend

	Return lisp.makeNil()
End Function


' ----------------------------------------------------------------------
' -- Execution
' ----------------------------------------------------------------------

Function LispMax_Builtin_Apply:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

'	lisp.printExpression(args)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local fn:LispMax_atom = args.car()
	args = args.cdar()

	If Not(args.isList()) Then
		Throw New Lispmax_SyntaxErrorException
	End If

	Return lisp.apply(fn, args)

End Function


' ----------------------------------------------------------------------
' -- String Functions
' ----------------------------------------------------------------------

Function LispMax_Builtin_StringUpcase:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' Throw an error if more than one argument passed in
	' TODO: Improve this
	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	If args.car().atom_type <> LispMax_Atom.ATOM_TYPE_STRING Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return lisp.MakeString(args.car().value_symbol.ToUpper())

End Function

Function LispMax_Builtin_StringDowncase:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	If args.car().atom_type <> LispMax_Atom.ATOM_TYPE_STRING Then
		Throw New Lispmax_UnexpectedTypeException
	End If

	Return lisp.MakeString(args.car().value_symbol.ToLower())

End Function


' ----------------------------------------------------------------------
' -- Predicates
' ----------------------------------------------------------------------

Function LispMax_Builtin_ListP:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	if args.car().isList() then
		Return lisp.makeSymbol("T")
	Else
		Return LispMax.makeNil()
	End If

End Function

Function LispMax_Builtin_Pair:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	If args.car().atom_type = LispMax_Atom.ATOM_TYPE_PAIR Then
		Return lisp.makeSymbol("T")
	Else
		Return LispMax.makeNil()
	End If

End Function

Function LispMax_Builtin_EQ:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cddr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdar()
	Local equal:Byte		= False

	If a.atom_type = b.atom_type Then

		Select a.atom_type

			Case LispMax_Atom.ATOM_TYPE_NIL
				equal = True

			Case LispMax_Atom.ATOM_TYPE_PAIR, ..
				 LispMax_Atom.ATOM_TYPE_CLOSURE, ..
				 LispMax_Atom.ATOM_TYPE_MACRO

				equal = a.value_pair = b.value_pair

			Case LispMax_Atom.ATOM_TYPE_SYMBOL, ..
				 LispMax_Atom.ATOM_TYPE_STRING

				equal = (a.value_symbol = b.value_symbol)

			Case LispMax_Atom.ATOM_TYPE_INTEGER
				equal = (a.value_number = b.value_number)

			Case LispMax_Atom.ATOM_TYPE_BUILTIN
				equal = (a.value_builtin = b.value_builtin)

		End Select

	End If

	If equal Then Return lisp.makeSymbol("T") Else Return lisp.makeNil()

End Function

Function LispMax_Builtin_IsNil:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
	EndIf

	If args.car().atom_type = LispMax_Atom.ATOM_TYPE_NIL Then
		Return lisp.makeSymbol("T")
	Else
		Return LispMax.makeNil()
	End If

End Function


' ----------------------------------------------------------------------
' -- List Functions
' ----------------------------------------------------------------------

Function LispMax_Builtin_List:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	Return args
End Function

Function LispMax_Builtin_FoldR:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' What does foldr(func, init, lst) do
	' if lst is empty, return init
	' Otherwise, call func with the parameters (lst.first, (foldr func init lst.rest))

	' So (foldr '+ 0 '(1 2 3))
	'   1. (+ 1 (foldr '+ 0 '(2 3)))
	'   2. (+ 1 (foldr '+ 0 '(2 3)))
	'
	'


	' args:
	' 0 = the function to call
	' 1 = the initial result
	' 2 = list of parameters
	Local func:Lispmax_Atom	= args.car()
	Local init:Lispmax_Atom	= args.cdar()
	Local list:Lispmax_Atom	= args.cddar()

	' Return initial result if at the end of the list.
	If False = list.isList() Or list.isNil() Then
		Return init
	End If

	' Call the function.
	Local rest:LispMax_Atom    = lispmax.cons(func, lispmax.cons(init, lispmax.cons(list.cdr(), lispmax.makeNil())))
	Local newArgs:LispMax_Atom = lispMax.cons(list.car(), lispmax.cons(lispmax_builtin_foldr(lisp, rest), lispmax.makeNil()))

	Return lisp.apply(func, newArgs)

End Function

Function LispMax_Builtin_FoldR_Test:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' What does foldr(func, init, lst) do
	' if lst is empty, return init
	' Otherwise, call func with the parameters (lst.first, (foldr func init lst.rest))

	' So (foldr '+ 0 '(1 2 3))
	'   1. (+ 1 (foldr '+ 0 '(2 3)))
	'   2. (+ 1 (foldr '+ 0 '(2 3)))
	'
	'


	' args:
	' 0 = the function to call
	' 1 = the initial result
	' 2 = list of parameters
	Local func:Lispmax_Atom	= args.car()
	Local init:Lispmax_Atom	= args.cdar()
	Local list:Lispmax_Atom	= args.cddar()

	Print "FOLDR:"
	lisp.printExpression(func)
	lisp.printExpression(init)
	lisp.printExpression(list)

	rem
	Local listItem:LispMax_Atom
	While list.isNil() = False
		' Get the next item of the list.
		listItem = list.car()
		list     = list.cdr()
	Wend
	end rem

	' Return initial result if at the end of the list.
	If False = list.isList() Or list.isNil() Then
		Print "Reached the end of the list!"
		lisp.printExpression(init)
		Return init
	End If

	' Call the function.
	Local rest:LispMax_Atom    = lispmax.cons(func, lispmax.cons(init, lispmax.cons(list.cdr(), lispmax.makeNil())))

	Print "Args to send to new foldr: " + lisp.expressionToString(rest)

	Local newArgs:LispMax_Atom = lispMax.cons(list.car(), lispmax.cons(LispMax_Builtin_FoldR_Test(lisp, rest), lispmax.makeNil()))

	Print "Args to send to APPLY: " + lisp.expressionToString(newArgs)

	Return lisp.apply(func, newArgs)

End Function


' ----------------------------------------------------------------------
' -- Standard IO
' ----------------------------------------------------------------------

Function LispMax_Builtin_Print:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	' If nothing passed in, print nothing
	If args.isNil() Or args.car().isNil() Or Not(args.cdr().isNil()) Then
		Return LispMax.makeNil()
	EndIf

	If args.car().atom_type = LispMax_Atom.ATOM_TYPE_STRING Then
		Print args.car().value_symbol
	ElseIf args.car().atom_type = LispMax_Atom.ATOM_TYPE_INTEGER Then
		Print args.car().value_number
	Else
		lisp.printExpression(args.car())
	EndIf

	Return LispMax.makeNil()

End Function

Function LispMax_Builtin_Debuglog:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.car().isNil() Or Not(args.cdr().isNil()) Then
		DebugLog "NIL"
		Return lisp.makeNil()
	EndIf

	If args.car().atom_type = LispMax_Atom.ATOM_TYPE_STRING Then
		DebugLog args.car().value_symbol
	ElseIf args.car().atom_type = LispMax_Atom.ATOM_TYPE_INTEGER Then
		DebugLog args.car().value_number
	Else
		DebugLog lisp.expressionToString(args.car())
	EndIf

	Return args.car()

End Function

Function LispMax_Builtin_PrintExpression:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.car().isNil() Or Not(args.cdr().isNil()) Then
		Print "Throw New Lispmax_ArgumentException: " + lisp.expressionToString(args.cdr())

	EndIf

	lisp.printExpression(args.car())

	Return LispMax.makeNil()

End Function