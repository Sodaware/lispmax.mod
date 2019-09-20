' ------------------------------------------------------------------------------
' -- src/lispmax_environment.bmx
' --
' -- Holds all information about an environment within LispMax. An environment
' -- contains a collection of symbols, which can be any valid atom.
' --
' -- This file is part of sodaware.mod (https://www.sodaware.net/sodaware.mod/)
' -- Copyright (c) 2009-2017 Phil Newton
' --
' -- See LICENSE for full license information.
' ------------------------------------------------------------------------------


Type LispMax_Environment

	Field _parent:LispMax_Environment
	Field _lookup:LispMax_AtomMap


	' ----------------------------------------------------------------------
	' -- Getting / Setting values
	' ----------------------------------------------------------------------

	''' <summary>Set the value of SYMBOL to VALUE.</summary>
	Method set(symbol:LispMax_Atom, value:LispMax_Atom)
		Self._lookup.insert(symbol.value_symbol, value)
	End Method

	''' <summary>Get the value of SYMBOL.</summary>
	Method get:LispMax_Atom(symbol:LispMax_Atom)

		' Search for the value in this environment.
		Local value:LispMax_Atom = Self._lookup.valueForKey(symbol.value_symbol)

		' Return value if found.
		If value <> Null Then Return value

		' If this environment has no parent the symbol does not exist.
		If Self._parent = Null Then
			Throw Lispmax_UnboundSymbolException.Create(symbol.value_symbol)
		End If

		' Try searching the parent.
		Return Self._parent.get(symbol)

	End Method

	''' <summary>Get the value of SYMBOL, or set it to defaultValue if not present.</summary>
	''' <param name="symbol">The symbol to retrieve.</param>
	''' <param name="defaultValue">The value to set if symbol is not found.</param>
	''' <param name="original">The environment that was first searched. Don't pass this in.</param>
	''' <return>The value for the symbol.</return>
	Method getOrSet:LispMax_Atom(symbol:LispMax_Atom, defaultValue:LispMax_Atom, original:LispMax_Environment = Null)

		' Set the original environment to the current level if not set. This
		' prevents symbols from being set in the global environment when they
		' aren't found.
		If original = Null Then original = Self

		' Search for the value in this environment and return if found.
		Local value:LispMax_Atom = Self._lookup.valueForKey(symbol.value_symbol)
		If value <> Null Then Return value

		' If this environment has no parent the symbol does not exist - set it and return it.
		If Self._parent = Null Then
			original.set(symbol, defaultValue)

			Return defaultValue
		End If

		' Try searching the parent.
		Return Self._parent.getOrSet(symbol, defaultValue, original)

	End Method

	''' <summary>Remove SYMBOL from the environment.</summary>
	Method remove(symbol:LispMax_Atom)
		Self._lookup.Remove(symbol.value_symbol)
	End Method

	''' <summary>Remove SYMBOL from the environment using its name. Don't do this.</summary>
	Method removeByName(symbolName:String)
		Self._lookup.remove(symbolName)
	End Method


	' ----------------------------------------------------------------------
	' -- Output
	' ----------------------------------------------------------------------

	Method toString:String()

		Local output:String = "(~n"

		If Self._parent <> Null Then output:+ Self._parent.toString()

		For Local symbol:String = EachIn Self._lookup.keys()

			Local atom:LispMax_Atom = Self._lookup.valueForKey(symbol)

			' [todo] - This should probably never happen
			If atom = Null Then
				DebugLog symbol + " is null!"
			Else
				Select atom.atom_type

					Case LispMax_Atom.ATOM_TYPE_SYMBOL
						output :+ "  (" + symbol + " . " + atom.value_symbol + ")~n"

					Case LispMax_Atom.ATOM_TYPE_BUILTIN
						output :+ "  (" + symbol + " . <#BUILTIN>)~n"

					Case LispMax_Atom.ATOM_TYPE_STRING
						output :+ "  (" + symbol + " . ~q" + atom.value_symbol + "~q)~n"

					Case LispMax_Atom.ATOM_TYPE_CLOSURE
						output :+ "  (" + symbol + " . <#CLOSURE>)~n"

					Case LispMax_Atom.ATOM_TYPE_INTEGER
						output :+ "  (" + symbol + " . " + atom.value_number + ")~n"

					Case LispMax_Atom.ATOM_TYPE_MACRO
						output :+ "  (" + symbol + " . <#MACRO>)~n"

					Default
						output :+ "  (" + symbol + " . " + atom.ToString() + ")~n"

				End Select

			EndIf
		Next

		Return output + ")"

	End Method


	' ----------------------------------------------------------------------
	' -- Construction / Destruction
	' ----------------------------------------------------------------------

	''' <summary>
	''' Create a new environment. Can optionally be passed a parent environment.
	''' </summary>
	Function Create:LispMax_Environment(parent:LispMax_Environment = Null)
		Local this:LispMax_Environment = New LispMax_Environment
		this._parent = parent
		Return this
	End Function

	Method New()
		Self._lookup = New LispMax_AtomMap
	End Method

End Type
