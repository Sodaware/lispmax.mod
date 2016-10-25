' ------------------------------------------------------------------------------
' -- src/lispmax_exceptions.bmx
' -- 
' -- All exceptions that can be thrown during execution.
' ------------------------------------------------------------------------------


SuperStrict

''' <summary>Base exception that LispMax will throw</summary>
Type LispMax_Exception Extends TBlitzException
End Type

Type LispMax_NullStackException Extends LispMax_Exception
	Method ToString:String()
		Return "Null stack during execution"
	End Method
End Type

''' <summary>Syntax error in execution.</summary>
Type LispMax_SyntaxErrorException Extends LispMax_Exception

	Field _message:String

	Function Create:Lispmax_SyntaxErrorException(message:String)
		Local this:Lispmax_SyntaxErrorException = New Lispmax_SyntaxErrorException
		this._message = message
		Return this
	End Function

	Method ToString:String()
		If Self._message = "" Then Return "Syntax Error"
		Return "Syntax Error: " + Self._message
	End Method
End Type

Type LispMax_UnboundSymbolException Extends LispMax_Exception

	Field _symbol:String
	
	Function Create:Lispmax_UnboundSymbolException(symbol:String)
		Local this:Lispmax_UnboundSymbolException = New Lispmax_UnboundSymbolException
		this._symbol = symbol
		Return this
	End Function
	
	Method ToString:String()
		Return "Attempted to evaluate symbol ~q" + Self._symbol + "~q, for which no binding exists"
	End Method

End Type

Type LispMax_ArgumentException Extends LispMax_Exception
	Method ToString:String()
		Return "A list expression was shorter or longer than expected"
	End Method
End Type

Type LispMax_UnexpectedTypeException Extends LispMax_Exception
	
	field _source:String
	field _arg:String
	field _expected:int
	Field _actual:Int
	
	Function Create:Lispmax_UnexpectedTypeException(source:String, expected:Int, actual:Int, arg:String = "")
		Local this:Lispmax_UnexpectedTypeException = New Lispmax_UnexpectedTypeException
		this._source	= source
		this._expected	= expected
		this._actual  	= actual
		this._arg		= arg
		Return this
	End Function

	Method ToString:String()
		
		If Self._arg <> "" And _expected <> "" Then
			Return "Operator ~q" + Self._source + "~q expected argument ~q" + Self._arg + "~q to be type ~q" + GetAtomTypeAsString(Self._expected) + "~q, ~q" + GetAtomTypeAsString(Self._actual) + "~q given."
		ElseIf _expected <> "" Then
			Return "Operator ~q" + Self._source + "~q expected a type of ~q" + GetAtomTypeAsString(Self._expected) + "~q, got ~q" + GetAtomTypeAsString(Self._actual) + "~q"
		Else
			Return "An object in an expression was of a different type than expected"
		End If
		
	End Method
	
End Type

Type LispMax_MissingArgumentException Extends LispMax_Exception
	
	field _source:String
	field _arg:String
	Field _message:String

	Function Create:Lispmax_MissingArgumentException(source:String, argName:String)
		Local this:Lispmax_MissingArgumentException = New Lispmax_MissingArgumentException
		this._source	= source
		this._arg		= argName
		Return this
	End Function

	Method ToString:String()
		
		If Self._arg <> "" then
			Return "Operator ~q" + Self._source + "~q is missing argument ~q" + Self._arg + "~q"
		Else
			Return "Operator ~q" + self._source + "~q did not receive enough arguments"
		End If
		
	End Method
	
End Type

Private

' UGLY!
Function GetAtomTypeAsString:String(atomType:Int)
	
	Select atomType
		Case 1	; Return "INTEGER"
		Case 2	; Return "SYMBOL"
		Case 3	; Return "NIL"
		Case 4	; Return "PAIR"
		Case 5	; Return "BUILTIN"
		Case 6	; Return "CLOSURE"
		Case 7	; Return "MACRO"
		Case 8	; Return "STRING"
		Default	; Return "Unknown [" + atomType + "]"
	End Select
		
End Function