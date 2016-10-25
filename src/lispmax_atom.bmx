' Atoms are the building blocks for all Lisp expressions

Type LispMax_Atom

	Global total_created:Int 	= 0
	
	Const ATOM_TYPE_INTEGER:Int = 1
	Const ATOM_TYPE_SYMBOL:Int	= 2
	Const ATOM_TYPE_NIL:Int 	= 3
	Const ATOM_TYPE_PAIR:Int 	= 4
	Const ATOM_TYPE_BUILTIN:Int	= 5
	Const ATOM_TYPE_CLOSURE:Int = 6
	Const ATOM_TYPE_MACRO:Int 	= 7
	Const ATOM_TYPE_STRING:Int	= 8
	
	Const SYMBOL_QUOTE:Int		= 1
	Const SYMBOL_DEFMACRO:Int	= 2
	Const SYMBOL_DEFINE:Int		= 3
	Const SYMBOL_PROGN:Int		= 4
	Const SYMBOL_SETQ:Int		= 5
	Const SYMBOL_LAMBDA:Int		= 6
	Const SYMBOL_IF:Int			= 7
	Const SYMBOL_APPLY:Int		= 8
	Const SYMBOL_SUSPEND:Int	= 9
	
	Field atom_type:Byte
	Field special_symbol:Byte
	
	Field value_pair:LispMax_Pair
	Field value_symbol:String
	Field value_number:Long
	Field value_builtin:Lispmax_Callable
	
	' Internal environment - only used by Callable types
	Field _environment:LispMax_Environment
	
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
	' -- Pair Helpers
	' ----------------------------------------------------------------------
	
	Method car:LispMax_Atom(value:LispMax_Atom = null)
		if value then self.value_pair.atom[0] = value
		Return Self.value_pair.atom[0]
	End Method

	Method cdr:LispMax_Atom(value:LispMax_Atom = null)
		if value then self.value_pair.atom[1] = value
		Return Self.value_pair.atom[1]
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Predicates
	' ----------------------------------------------------------------------
		
	Method nilp:Byte()
		Return Self.atom_type = LispMax_Atom.ATOM_TYPE_NIL
	End Method
	
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
	
	Method isNil:Byte()
		Return Self.nilp()
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Type Checking
	' ----------------------------------------------------------------------
			
	Method isAtomType:Byte(atomType:Int)
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
	' -- Creation / Destruction
	' ----------------------------------------------------------------------
		
	' [todo] - Don't want to do this, it makes every atom 3 times larger than it needs to be!
	Method New()
		LispMax_Atom.total_created:+ 1
		Self.value_pair = New LispMax_Pair
	End Method
	
	Method Destroy()
		DebugLog "Object sploded"
	End Method
	
End Type

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

Type Lispmax_Builtin Extends Lispmax_Callable
	
	Field _handler:LispMax_Atom(lisp:LispMax, args:Lispmax_Atom)
	
	Method call:LispMax_Atom(caller:LispMax, args:LispMax_Atom)
		Return Self._handler(caller, args)
	End Method
	
End Type

Type Lispmax_Callable Abstract
	
	Method call:LispMax_Atom(caller:LispMax, args:LispMax_Atom) Abstract
	
End Type
