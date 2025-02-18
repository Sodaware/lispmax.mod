' ------------------------------------------------------------------------------
' -- src/lispmax_atom.bmx
' --
' -- Atoms are the building blocks for all Lisp expressions. An atom can be a
' -- number of things, such as a string, symbol or integer.
' --
' -- This file is part of lispmax (https://www.sodaware.net/lispmax/)
' -- Copyright (c) 2017-2019 Phil Newton
' --
' -- See COPYING for full license information.
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

	' Special symbols that are evaluated internally.
	Const SYMBOL_QUOTE:Byte		 = 1
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

	' Pool support.
	Field _next:LispMax_Atom

	' Internal environment - only used by Callable types
	' TODO: Can we move this to the callable type instead?
	Field _environment:LispMax_Environment


	' ----------------------------------------------------------------------
	' -- Pair Helpers
	' ----------------------------------------------------------------------

	Method car:LispMax_Atom(value:LispMax_Atom = Null)
		If value Then Self.value_pair.atom[LISPMAX_CAR] = value

		Return Self.value_pair.atom[LISPMAX_CAR]
	End Method

	Method cdr:LispMax_Atom(value:LispMax_Atom = Null)
		If value Then Self.value_pair.atom[LISPMAX_CDR] = value

		Return Self.value_pair.atom[LISPMAX_CDR]
	End Method

	' car car
	Method caar:LispMax_Atom()
		Return Self.value_pair.atom[LISPMAX_CAR].value_pair.atom[LISPMAX_CAR]
	End Method

	' cdr cdr
	Method cddr:LispMax_Atom()
		Return Self.value_pair.atom[LISPMAX_CDR].value_pair.atom[LISPMAX_CDR]
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
	' -- Type Checking
	' ----------------------------------------------------------------------

	Method isAtomType:Byte(atomType:Byte)
		Return Self.atom_type = atomType
	End Method

	Method isNil:Byte()
		Return ATOM_TYPE_NIL = Self.atom_type
	End Method

	Method isPairAtom:Byte()
		Return ATOM_TYPE_PAIR = Self.atom_type
	End Method

	Method isSymbolAtom:Byte()
		Return ATOM_TYPE_SYMBOL = Self.atom_type
	End Method

	Method isMacroAtom:Byte()
		Return ATOM_TYPE_MACRO = Self.atom_type
	End Method

	' TODO: This can be slow for larger lists.
	Method isList:Byte()
		Local expression:LispMax_Atom = Self
		While expression.atom_type <> ATOM_TYPE_NIL
			If expression.atom_type <> ATOM_TYPE_PAIR Then
				Return False
			End If
			expression = expression.value_pair.atom[LISPMAX_CDR]
		Wend

		Return True
	End Method

	Method isCallable:Byte()
		Return Self.isAtomType(ATOM_TYPE_BUILTIN) Or Self.isAtomType(ATOM_TYPE_CLOSURE)
	End Method


	' ----------------------------------------------------------------------
	' -- Copying Atoms
	' ----------------------------------------------------------------------

	Method copy:LispMax_Atom()

		Local newAtom:LispMax_Atom = New LispMax_Atom

		newAtom.value_pair     = Self.value_pair.copy()
		newAtom.value_symbol   = Self.value_symbol
		newAtom.value_number   = Self.value_number
		newAtom.value_builtin  = Self.value_builtin
		newAtom.special_symbol = Self.special_symbol
		newAtom._environment   = Self._environment

		'If Self.car() <> Null And Self.car().isNil() = False Then newAtom.car(Self.car().copy())
		'If Self.cdr() <> Null And Self.cdr().isNil() = False Then newAtom.cdr(Self.cdr().copy())

		Return newAtom

	End Method


	' ----------------------------------------------------------------------
	' -- Creation / Destruction
	' ----------------------------------------------------------------------

	Method New()
		LispMax_Atom.total_created:+ 1
		Self.value_pair = New LispMax_Pair
	End Method

	' This doesn't get called until BMX decides to run it.
	Method Delete()
		LispMax_Atom.total_deleted:+ 1

		Self.value_pair.atom[0] = Null
		Self.value_pair.atom[1] = Null
	End Method

	''' <summary>Reset the atom to its initial state.</summary>
	Method reset()
		' Clear values/types.
		Self.atom_type      = 0
		Self.value_symbol   = ""
		Self.value_number   = 0
		Self.value_builtin  = Null
		Self.special_symbol = 0
		Self._environment   = Null

		' Clear the pair data.
		Self.value_pair.atom[0] = Null
		Self.value_pair.atom[1] = Null
	End Method

End Type


' ----------------------------------------------------------------------
' -- Sub-Types
' ----------------------------------------------------------------------

Type LispMax_Pair
	Field atom:LispMax_Atom[2]

	Method copy:LispMax_Pair()
		Local newPair:LispMax_Pair = New LispMax_Pair

		newPair.atom[0] = Self.atom[0]
		newPair.atom[1] = Self.atom[1]

		Return newPair
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

	Function Create:LispMax_Builtin(fn:LispMax_Atom(lisp:LispMax, args:Lispmax_Atom))
		Local this:LispMax_Builtin = New LispMax_Builtin

		this._handler = fn

		Return this
	End Function
End Type
