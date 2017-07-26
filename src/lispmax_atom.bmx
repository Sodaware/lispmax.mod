' ------------------------------------------------------------------------------
' -- src/lispmax_atom.bmx
' -- 
' -- Atoms are the building blocks for all Lisp expressions. An atom can be a
' -- number of things, such as a string, symbol or integer.
' ------------------------------------------------------------------------------


const LISPMAX_CAR:byte = 0
const LISPMAX_CDR:byte = 1

Type LispMax_Atom

	' Global statistics.
	Global total_created:Int     = 0
	Global total_deleted:Int     = 0
	
	Const ATOM_TYPE_INTEGER:Byte = 1
	Const ATOM_TYPE_SYMBOL:Byte	 = 2
	Const ATOM_TYPE_NIL:Byte     = 3
	Const ATOM_TYPE_PAIR:Byte    = 4
	Const ATOM_TYPE_BUILTIN:Byte = 5
	Const ATOM_TYPE_CLOSURE:Byte = 6
	Const ATOM_TYPE_MACRO:Byte   = 7
	Const ATOM_TYPE_STRING:Byte  = 8
	
	' Special symbols that are evaluated internally
	Const SYMBOL_QUOTE:Byte	     = 1
	Const SYMBOL_DEFMACRO:Byte   = 2
	Const SYMBOL_DEFINE:Byte     = 3
	Const SYMBOL_PROGN:Byte      = 4
	Const SYMBOL_SETQ:Byte       = 5
	Const SYMBOL_LAMBDA:Byte     = 6
	Const SYMBOL_IF:Byte         = 7
	Const SYMBOL_APPLY:Byte      = 8
	Const SYMBOL_SUSPEND:Byte    = 9
	Const SYMBOL_WHEN:Byte       = 10
	Const SYMBOL_UNLESS:Byte     = 11
	Const SYMBOL_DEFUN:Byte      = 12
	
	' -- Actual atom data
	Field atom_type:Byte
	Field special_symbol:Byte
	Field value_pair:LispMax_Pair
	Field value_symbol:String
	Field value_number:Long
	Field value_builtin:Lispmax_Callable
	
	' Internal environment - only used by Callable types
	Field _environment:LispMax_Environment
	
	
	' ----------------------------------------------------------------------
	' -- Pair Helpers
	' ----------------------------------------------------------------------
	
	Method car:LispMax_Atom(value:LispMax_Atom = Null)
		If value Then Self.value_pair.atom[0] = value
		Return Self.value_pair.atom[0]
	End Method

	''' <summary>Get the value of the atom's CAR.</summary>
	Method getCar:LispMax_Atom()
		Return Self.value_pair.atom[0]
	End Method

	Method getPair:LispMax_Pair()
		Return Self.value_pair
	End Method

	Method cdr:LispMax_Atom(value:LispMax_Atom = Null)
		If value Then Self.value_pair.atom[1] = value
		Return Self.value_pair.atom[1]
	End Method
	
	' car car
	Method caar:LispMax_Atom()
		Return Self.value_pair.atom[0].value_pair.atom[0]
	End Method
	
	' cdr cdr
	Method cddr:LispMax_Atom()
		Return Self.value_pair.atom[1].value_pair.atom[1]
	End Method
	
	' car car car
	Method caaar:LispMax_Atom()
		Return Self.value_pair.atom[0].value_pair.atom[0].value_pair.atom[0]
	End Method
	
	' cdr cdr cdr
	Method cdddr:LispMax_Atom()
		Return Self.value_pair.atom[1].value_pair.atom[1].value_pair.atom[1]
	End Method
	
	' car cdr
	Method cadr:LispMax_Atom()
		Return Self.value_pair.atom[0].value_pair.atom[1]
	End Method
	
	' cdr car
	Method cdar:LispMax_Atom()
		Return Self.value_pair.atom[1].value_pair.atom[0]
	End Method

    ' cdr cdr car
	Method cddar:LispMax_Atom()
		Return Self.value_pair.atom[1].value_pair.atom[1].value_pair.atom[0]
	End Method
	
		
	' ----------------------------------------------------------------------
	' -- Predicates
	' ----------------------------------------------------------------------
		
	Method nilp:Byte()
		Return Self.atom_type = LispMax_Atom.ATOM_TYPE_NIL
	End Method

	Method isNil:Byte()
		Return Self.atom_type = LispMax_Atom.ATOM_TYPE_NIL
	End Method
	
	' This can be a little slow...
	Method listp:Byte()
		
		Local expression:LispMax_Atom = Self
		While expression.isNil() = False
			If expression.atom_type <> ATOM_TYPE_PAIR Then
				Return False
			End If
			expression = expression.cdr()
		Wend
		
		Return True
			
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Type Checking
	' ----------------------------------------------------------------------
			
	Method isAtomType:Byte(atomType:Byte)
		Return Self.atom_type = atomType
	End Method
	
	Method isPairAtom:Byte()
		Return Self.isAtomType(ATOM_TYPE_PAIR)
	End Method
	
	Method isSymbolAtom:Byte()
		Return Self.isAtomType(ATOM_TYPE_SYMBOL)
	End Method
	
	Method isMacroAtom:Byte()
		Return Self.isAtomType(ATOM_TYPE_MACRO)
	End Method
	
	Method isList:Byte()
		Return Self.listp()
	End Method	
	
	Method isCallable:Byte()
		Return Self.isAtomType(ATOM_TYPE_BUILTIN) Or Self.isAtomType(ATOM_TYPE_CLOSURE)
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Copying Atoms
	' ----------------------------------------------------------------------
	
	Method copy:LispMax_Atom()
		
		Local newAtom:LispMax_Atom = New LispMax_Atom
		
		newAtom.value_pair 		= Self.value_pair.copy()
		newAtom.value_symbol	= Self.value_symbol
		newAtom.value_number 	= Self.value_number
		newAtom.value_builtin 	= Self.value_builtin
		newAtom.special_symbol	= Self.special_symbol
		newAtom._environment	= Self._environment
		
		'If Self.car() <> Null And Self.car().isNil() = False Then newAtom.car(Self.car().copy())
		'If Self.cdr() <> Null And Self.cdr().isNil() = False Then newAtom.cdr(Self.cdr().copy())
		
		Return newAtom
		
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Creation / Destruction
	' ----------------------------------------------------------------------
		
	' [todo] - Don't want to do this, it makes every atom 3 times larger than it needs to be!
	Method New()
		LispMax_Atom.total_created:+ 1
		Self.value_pair = New LispMax_Pair
	End Method
	
	Method Delete()
		LispMax_Atom.total_deleted:+ 1
		Self.value_pair.atom[0] = Null
		Self.value_pair.atom[1] = Null
	End Method
	
	Method Destroy()
		DebugLog "Object sploded"
	End Method
	
End Type


' ----------------------------------------------------------------------
' -- Sub-Types
' ----------------------------------------------------------------------

Type LispMax_Pair
	Field atom:LispMax_Atom[2]
	
	Method copy:LispMax_Pair()
		Local newPair:LispMax_Pair = New LispMax_Pair
		If Self.atom[0] <> Null Then newPair.atom[0] = Self.atom[0]
		If Self.atom[1] <> Null Then newPair.atom[1] = Self.atom[1]
		Return newPair
	End Method
	
	Method New()
		Self.atom = New LispMax_Atom[2]
	End Method
	
End Type

''' <summary>A callable atom (i.e. a function or lambda).</summary>
Type Lispmax_Callable Abstract
	Method call:LispMax_Atom(caller:LispMax, args:LispMax_Atom) Abstract
End Type

Type Lispmax_Builtin Extends Lispmax_Callable
	
	Field _handler:LispMax_Atom(lisp:LispMax, args:Lispmax_Atom)
	
	Method call:LispMax_Atom(caller:LispMax, args:LispMax_Atom)
		Return Self._handler(caller, args)
	End Method
	
End Type
