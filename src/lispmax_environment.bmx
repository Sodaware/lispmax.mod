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
	
	Method remove(symbol:LispMax_Atom)
		Self.removeByName(symbol.value_symbol)
	End Method
	
	Method removeByName(symbolName:String)
		Self._lookup.Remove(symbolName)
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Copying
	' ----------------------------------------------------------------------
	
	Method dump:String()
		
		Local output:String = "(~n"
	
		If Self._parent <> Null Then output:+ Self._parent.dump()
		
		For Local symbol:String = EachIn Self._lookup.Keys()
			
			Local atom:LispMax_Atom = LispMax_Atom(Self._lookup.ValueForKey(symbol))
			
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
		Self._lookup = New TMap
	End Method
	
End Type