' ----------------------------------------------------------------------
' -- Atom operations
' ----------------------------------------------------------------------

Function LispMax_Builtin_Car:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or Not(args.cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	If args.car().isNil() Then
		Return LispMax_Nil
	ElseIf args.car().atom_type <> LispMax_Atom.ATOM_TYPE_PAIR Then
		throw Lispmax_UnexpectedTypeException.create("CAR", Lispmax_Atom.ATOM_TYPE_PAIR, args.car().atom_type)
	End If
	
	Return args.car().car()
	
End Function

Function LispMax_Builtin_Cdr:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or Not(args.cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	If args.car().isNil() Then
		Return LispMax_Nil
	ElseIf args.car().atom_type <> LispMax_Atom.ATOM_TYPE_PAIR Then
		throw Lispmax_UnexpectedTypeException.create("CDR", Lispmax_Atom.ATOM_TYPE_PAIR, args.car().atom_type)
	End If
	
	Return args.car().cdr()
	
End Function

Function LispMax_Builtin_Cons:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Return lisp.cons(args.car(), args.cdr().car())
	
End Function

' [todo] - Need some error checking here!

Function LispMax_Builtin_Nth:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local value:LispMax_Atom = args.cdr().car()
	For Local i:Int = 1 To args.car().value_number
		value = value.cdr()
	Next
	
	Return value.car()
	
	'Return lisp.cons(args.car(), args.cdr().car())
	
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
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then
		lisp.printExpression(args)
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return LispMax.MakeInt(a.value_number + b.value_number)
	
End Function

Function LispMax_Builtin_Subtract:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return LispMax.MakeInt(a.value_number - b.value_number)
	
End Function

Function LispMax_Builtin_Multiply:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return LispMax.MakeInt(a.value_number * b.value_number)
	
End Function

Function LispMax_Builtin_Divide:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return LispMax.MakeInt(a.value_number / b.value_number)
	
End Function

Function LispMax_Builtin_Mod:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return LispMax.MakeInt(a.value_number mod b.value_number)
	
End Function

Function LispMax_Builtin_NumEQ:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Local result:LispMax_Atom = LispMax.makeNil()
	If a.value_number = b.value_number Then result = lisp.makeSymbol("T")
	
	Return result
	
End Function

Function LispMax_Builtin_NumLT:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Local result:LispMax_Atom = LispMax.makeNil()
	If a.value_number < b.value_number Then result = lisp.makeSymbol("T")
	
	Return result
	
End Function

Function LispMax_Builtin_NumGT:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
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
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
	
	If a.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Or b.atom_type <> LispMax_Atom.ATOM_TYPE_INTEGER Then
		Throw New Lispmax_UnexpectedTypeException
	End If
	
	Return lisp.makeNumber(Rand(a.value_number, b.value_number))
	
End Function

Function LispMax_Builtin_Millisecs:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	Return lisp.makeNumber(MilliSecs())
End Function


' ----------------------------------------------------------------------
' -- Execution
' ----------------------------------------------------------------------

Function LispMax_Builtin_Apply:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)
	
	lisp.printExpression(args)

	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local fn:LispMax_atom = args.car()
	args = args.cdr().car()
	
	If Not(args.isList()) Then
		Throw New Lispmax_SyntaxErrorException
	End If
	
	Return lisp.apply(fn, args)
	
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
	
	If args.isNil() Or args.cdr().isNil() Or Not(args.cdr().cdr().isNil()) Then 
		Throw New Lispmax_ArgumentException
	EndIf
	
	Local a:Lispmax_Atom	= args.car()
	Local b:Lispmax_Atom	= args.cdr().car()
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
' -- Standard IO
' ----------------------------------------------------------------------

Function LispMax_Builtin_Print:LispMax_Atom(lisp:LispMax, args:LispMax_Atom)

	If args.isNil() Or args.car().isNil() Or Not(args.cdr().isNil()) Then
		Throw New Lispmax_ArgumentException
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
