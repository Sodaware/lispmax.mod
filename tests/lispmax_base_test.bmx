SuperStrict

Import sodaware.LispMax

Type LispMax_BaseTest Extends TTest Abstract
    
    Field _lisp:LispMax
	
    Method setup() { before }
        Self._lisp  = New LispMax
        Self._lisp.initializeEnvironment()
    End Method
    
    Method tearDown() { after }
        Self._lisp = Null
        GCCollect()
    End Method
	
	
    ' ----------------------------------------------------------------------
    ' -- Helpers
    ' ----------------------------------------------------------------------
    
    Method runExpression:Lispmax_Atom(content:String)
    
        Local expression:LispMax_Atom = Self._lisp.parseExpression(content)
        Return Self._lisp.evaluateExpression(expression, Self._lisp._environment)
    
    End Method
    
    Method assertLispNil(expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)

        if result = null or result <> LispMax.makeNil() then
            fail("assertLispNil() : ~q" + expression + "~q returned ~q" + result.value_symbol + "~q instead of NIL" )
        endif

    End Method  

    Method assertLispTrue(expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)

        if result = null or result.value_symbol <> "T" then
            fail("assertLispTrue() : ~q" + expression + "~q returned ~q" + result.value_symbol + "~q instead of T" )
        endif

    End Method

    Method assertLispEqualsValue(value:Double, expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)

        if result = null or result.value_number <> value then
            fail("assertLispEqualsValue() : ~q" + expression + "~q returned ~q" + result.value_number + "~q instead of ~q" + value + "~q" )
        endif

    End Method
	
	Method assertLispStringEquals(value:String, expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)
		
		If result = Null Or Not(result.isAtomType(LispMax_Atom.ATOM_TYPE_STRING)) Then
			fail("assertLispStringEquals() : ~q" + expression + "~q did not return a string value")
		End If
		
        If result = Null Or result.value_symbol <> value Then
            fail("assertLispStringEquals() : ~q" + expression + "~q returned ~q" + result.value_symbol + "~q instead of ~q" + value + "~q" )
        endif

    End Method
    
    Method assertLispGreaterThanValue(value:Double, expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)

        If result = Null Or result.value_number <= value Then
            fail("assertLispGreaterThanValue() : ~q" + expression + "~q returned ~q" + result.value_number + "~q which is less than ~q" + value + "~q" )
        endif

    End Method
    
    Method assertLispLessThanValue(value:Double, expression:String)

        Local result:Lispmax_Atom = self.runExpression(expression)

        If result = Null Or result.value_number >= value Then
            fail("assertLispLessThanValue() : ~q" + expression + "~q returned ~q" + result.value_number + "~q which is greater than than ~q" + value + "~q" )
        EndIf

    End Method
    
End Type
