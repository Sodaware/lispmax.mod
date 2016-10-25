SuperStrict

Import brl.linkedlist
Import sodaware.blitzmax_ascii
Import brl.standardio

Type LispMax_Lexer
	
	Global CHAR_LOOKUP:String[256]
	
	' Internal stuff
	Field _pos:Int = 0
	Field _buffer:String = ""
	
	Field _currentToken:LispMax_Token
	Field _previousToken:LispMax_Token
	Field _tokens:TList
	Field _tokenPosition:Int
	
	Method getCurrentToken:LispMax_Token()
	End Method

	Method getToken(position:int = 0)
	End Method

	Method readAll()
	End Method

	Method peekToken:LispMax_Token()
		local token:LispMax_Token = self.readToken()
		self._pos :- 1
		return token
	End Method
	
	Method peekChar:Byte(offset:Int = 0)
		Return Self._buffer[Self._pos + offset]
	End Method

	''
	' Reads a token from the source and moves the internal pointer forward.
	Method readToken:LispMax_Token()
		
		' Check for end of file
		if self._pos >= self._buffer.Length then 
			Return LispMax_Token.Create(LispMax_Token.TOKEN_EOF, "//EOF//", Self._pos)
		endif

		' Get the current character
		Local currentChar:Byte = Self._buffer[Self._pos]
		
		' Read all characters up to null terminator or end of token
		While currentChar <> ASC_NULL
			
			' Absorb comments
			If currentChar = ASC_SEMI_COLON Then
				
				Repeat

					' Move to next character
					Self._pos:+ 1

					' If end of file, we're done
					If Self._pos >= Self._buffer.Length Then
						Return LispMax_Token.Create(LispMax_Token.TOKEN_EOF, "//EOF//", Self._pos)
					EndIf
					
					currentChar = Self._buffer[Self._pos]
					
					If currentChar = ASC_NULL Then
						' Throw "Lexical error - exited too early"
						Return LispMax_Token.Create(LispMax_Token.TOKEN_EOF, "//EOF//", Self._pos)
					endif

				Until currentChar = ASC_LF
				
			EndIf
			
			' Check for end of file
			If currentChar = ASC_NULL Then
				Return LispMax_Token.Create(LispMax_Token.TOKEN_EOF, "", Self._pos)
			EndIf
			
			' Read token
			Select currentChar
				Case ASC_PERIOD	; Self._pos :+ 1 ; Return LispMax_Token.Create(LispMax_Token.TOKEN_PERIOD, ".", Self._pos - 1)
				Case ASC_BRACKET_OPEN	; Self._pos :+ 1 ; Return LispMax_Token.Create(LispMax_Token.TOKEN_OPEN_PAREN, "(", Self._pos - 1)
				Case ASC_BRACKET_CLOSE	; Self._pos :+ 1 ; Return LispMax_Token.Create(LispMax_Token.TOKEN_CLOSE_PAREN, ")", Self._pos - 1)
				Case ASC_APOSTROPHE	; Self._pos :+ 1 ; Return LispMax_Token.Create(LispMax_Token.TOKEN_QUOTE, "'", Self._pos - 1)
				Case ASC_QUOTE	; Self._pos :+ 1 ; Return Self.readStringToken()
				Case ASC_GRAVE	; Self._pos :+ 1 ; Return LispMax_Token.Create(LispMax_Token.TOKEN_SPECIAL_QUOTE, "`", Self._pos - 1)
				Case ASC_COMMA
					If Self.peekChar(1) = ASC_AT Then
						Self._pos :+ 2
						Return LispMax_Token.Create(LispMax_Token.TOKEN_SPECIAL_QUOTE, ",@", Self._pos - 1)
					Else
						Self._pos :+ 1
						Return LispMax_Token.Create(LispMax_Token.TOKEN_SPECIAL_QUOTE, ",", Self._pos - 1)
					End If
			End Select
			
			' Check for numbers
			If Self.isDigit(currentChar) Then
				Return Self.readNumberToken()
			End If
			
			If currentChar = ASC_MINUS And Self.isdigit(Self.peekChar(1)) Then
				Return Self.readNumberToken()
			End If
			
			If Self.isSymbolCharacter(currentChar) Then
				Return Self.readSymbolToken()
			End If
			
			' Skip empty space character
			Self._pos :+ 1

			' Check for end of file
			if self._pos >= self._buffer.Length then 
				Return LispMax_Token.Create(LispMax_Token.TOKEN_EOF, "//EOF//", Self._pos)
			endif

			currentChar = Self._buffer[Self._pos]
			
		Wend
		
		
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Token Readers
	' ----------------------------------------------------------------------
	
	''' <summary>Reads a string and returns it as a token.</summary>
	Method readStringToken:LispMax_Token()
	
		Local v:String = ""
		
		If Self._pos >= Self._buffer.Length Then
			Throw "Unterminated string"
		EndIf
		
		Local currentChar:Byte = Self._buffer[Self._pos]
		
		While currentChar <> 0 And currentChar <> ASC_QUOTE
		
			v:+ LispMax_Lexer.CHAR_LOOKUP[currentChar]
			
			Self._pos:+ 1
			If Self._pos >= Self._buffer.Length Then
				Throw "Unterminated string"
			EndIf
			
			currentChar = Self._buffer[Self._pos]
			
		Wend
		
		Self._pos:+ 1
		
		Return LispMax_Token.Create(LispMax_Token.TOKEN_LITERAL, v, Self._pos - 1)
		
	End Method
	
	''' <summary>Reads a number and returns it as a token.</summary>
	Method readNumberToken:LispMax_Token()
	
		Local v:String = ""
		Local currentChar:Byte = Self._buffer[Self._pos]

		Repeat

			' Add current digit to the number
			v:+ LispMax_Lexer.CHAR_LOOKUP[currentChar]
			
			' Move to the next character
			Self._pos:+ 1

			' Check character is within the bounds
			If Self._pos >= Self._buffer.Length Then 
				Throw "Unterminated number"
			EndIf

			' Get the next character
			currentChar = Self._buffer[Self._pos]
			
		Until currentChar = ASC_NULL Or Not(Self.isdigit(currentChar) Or Self.isPeriod(currentChar))

		' Parse value into a number and return the token
		Return LispMax_Token.Create(LispMax_Token.TOKEN_NUMBER, Long(v), Self._pos - 1)
				
	End Method
	
	''' <summary>Reads a symbol and returns it as a token.</summary>
	Method readSymbolToken:LispMax_Token()
	
		Local v:String = ""
		Local currentChar:Byte = Self._buffer[Self._pos]
		
		Repeat

			' Add current character to the symbol
			v:+ LispMax_Lexer.CHAR_LOOKUP[currentChar]
			
			Self._pos:+ 1
			If Self._pos >= Self._buffer.Length Then
				Throw "Unterminated symbol"
			EndIf

			' Get the next character
			currentChar = Self._buffer[Self._pos]

		Until currentChar = ASC_NULL Or Not(Self.isSymbolCharacter(currentChar))
		
		' Return the read symbol
		Return LispMax_Token.Create(LispMax_Token.TOKEN_SYMBOL, v, Self._pos - 1)
		
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Character Checks
	' ----------------------------------------------------------------------
	
	Method isDigit:Byte(c:Byte)
		Return (c >= 48 And c <= 57)
	End Method
	
	Method isPeriod:Byte(c:Int)
		Return c = ASC_PERIOD
	End Method

	Method isSymbolCharacter:Byte(c:Int)
		
		' Not as elegant as the previous way of doing things, but it's about twice
		' as fast. Evaluation order is planned so most likely failures will skip 
		' out first.
				
		Select c
			
			Case ASC_SPACE				; Return False
			Case ASC_TAB				; Return False
			Case ASC_LF					; Return False
			Case ASC_CR					; Return False
			Case ASC_BRACKET_OPEN		; Return False
			Case ASC_BRACKET_CLOSE		; Return False
			Case ASC_APOSTROPHE			; Return False
			Case ASC_QUOTE				; Return False
			Case ASC_COMMA				; Return False
			Case ASC_GRAVE				; Return False
			Case ASC_CURLY_OPEN			; Return False
			Case ASC_CURLY_CLOSE		; Return False
			Case ASC_SQUARE_OPEN		; Return False
			Case ASC_SQUARE_CLOSE		; Return False
			Case ASC_SEMI_COLON			; Return False
			Case ASC_HASH				; Return False
			Case ASC_PIPE				; Return False
			Case ASC_BACKSLASH			; Return False
			
			Default						; Return True
				 
		End Select
		
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Construction
	' ----------------------------------------------------------------------
		
	''
	' Sets up a lexer with some source
	Function Create:LispMax_Lexer(source:String)
		Local this:LispMax_Lexer = New Lispmax_lexer
		this._buffer = source
		this._pos    = 0
		Return this
	End Function
	
	Method New()
		Self._tokens = New TList
	End Method
	
End Type


Type LispMax_Token
	
	' Token Definitions
	Const TOKEN_OPEN_PAREN:Int 		= 1
	Const TOKEN_CLOSE_PAREN:Int 	= 2
	Const TOKEN_LITERAL:Int 		= 3
	Const TOKEN_QUOTE:Int 			= 4
	Const TOKEN_ERROR:Int 			= 5
	Const TOKEN_EOF:Int 			= 6
	Const TOKEN_PERIOD:Int 			= 7
	Const TOKEN_NUMBER:Int			= 8
	Const TOKEN_SYMBOL:Int			= 9
	Const TOKEN_SPECIAL_QUOTE:Int 	= 10
	
	Field tokenType:Int
	Field position:Int
	Field contents:String
	
	Function Create:LispMax_Token(tokenType:Int, contents:String, position:Int)
		
		Local this:LispMax_Token = New LispMax_token
		
		this.tokenType = tokenType
		this.contents  = contents
		this.position  = position
		
		Return this
		
	End Function
	
	
End Type

Private

LispMax_Lexer.CHAR_LOOKUP = New String[256]

For Local i:Int = 0 To 255
	LispMax_Lexer.CHAR_LOOKUP[i] = Chr(i)
Next