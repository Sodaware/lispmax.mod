SuperStrict

Type Lispmax_SyntaxErrorException Extends TBlitzException
	Method ToString:String()
		Return "Syntax Error"
	End Method
End Type

Type Lispmax_UnboundSymbolException Extends TBlitzException

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

Type Lispmax_ArgumentException Extends TBlitzException
	Method ToString:String()
		Return "A list expression was shorter or longer than expected"
	End Method
End Type

Type Lispmax_UnexpectedTypeException Extends TBlitzException
	
	field _source:String
	field _arg:String
	field _expected:int
	field _actual:int

	Function Create:Lispmax_UnexpectedTypeException(source:String, expected:int, actual:int)
		Local this:Lispmax_UnexpectedTypeException = New Lispmax_UnexpectedTypeException
		this._source	= source
		this._expected = expected
		this._actual   = actual
		Return this
	End Function

	Method ToString:String()
		if _expected <> "" then
			Return "Operator ~q" + self._source + "~q expected a type of ~q" + self._expected + "~q, got ~q" + self._actual + "~q"
		else
			Return "An object in an expression was of a different type than expected"
		End If
	End Method
End Type