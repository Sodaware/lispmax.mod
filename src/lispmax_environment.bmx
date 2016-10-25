' ------------------------------------------------------------------------------
' -- lispmax_environment.bmx
' -- 
' -- Holds all information about an environment within LispMax. An environment
' -- contains a collection of symbols, which can be any valid atom.
' ------------------------------------------------------------------------------


Type LispMax_Environment

	Field _parent:LispMax_Environment
	Field _lookup:TMap
	
	
	' ----------------------------------------------------------------------
	' -- Copying
	' ----------------------------------------------------------------------
	
	Method copy:LispMax_Environment()
		Local this:LispMax_Environment = New LispMax_Environment
		this._parent = Self._parent
		For Local k:Object = EachIn Self._lookup.keys()
			this._lookup.Insert(k, Self._lookup.ValueForKey(k))
		Next
		Return this
	End Method
	
		
	' ----------------------------------------------------------------------
	' -- Getting / Setting values
	' ----------------------------------------------------------------------
	
	''' <summary>Set the value of SYMBOL to VALUE.</summary>
	Method set(symbol:LispMax_Atom, value:LispMax_Atom)
		Self._lookup.Insert(symbol.value_symbol, value)
	End Method
	
	Method get:LispMax_Atom(symbol:LispMax_Atom)
		
		Local value:LispMax_Atom
		
		' Search for the value in this environment
		value = LispMax_Atom(Self._lookup.ValueForKey(symbol.value_symbol))
		
		' Return value if found
		If value <> Null Then
			Return value
		EndIf
		
		' If no parent, then symbol does not exist
		If Self._parent = Null Then
			Throw Lispmax_UnboundSymbolException.Create(symbol.value_symbol)
		End If
		
		' Try searching the parent
		Return Self._parent.get(symbol)
		
	End Method
	

	
		
	
	' ----------------------------------------------------------------------
	' -- Copying
	' ----------------------------------------------------------------------
	
	Method dump:String()
		
		Local output:String = "(~n"
	
		If Self._parent <> Null Then output:+ Self._parent.dump()
		
		For Local symbol:String = EachIn Self._lookup.Keys()
			' [todo] - This should probably never happen
			If Self._lookup.ValueForKey(symbol) = Null Then
				DebugLog symbol + " is null!"
			Else 
				output :+ "  (" + symbol + " . " + LispMax_Atom(Self._lookup.ValueForKey(symbol)).value_symbol + ")~n"
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
		Self._lookup = New TMap
	End Method
	
End Type