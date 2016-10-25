SuperStrict

Framework BaH.MaxUnit
Import sodaware.LispMax

' run the tests!
New TTestSuite.run()

Type Sodaware_Lispmax_Tests Extends TTest
	
	Field _lisp:LispMax

	Method setup() { before }
		Self._lisp	= New LispMax
		Self._lisp.initializeEnvironment()
	End Method
	
	Method tearDown() { after }
		Self._lisp = Null
		GCCollect()
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Atom tests
	' ----------------------------------------------------------------------
	
	Method testCanUseFloatingPoint() { test }
		Self.assertEqualsL(0.1, Self.runExpression("0.1").value_number)
		Self.assertEqualsL(0.5, Self.runExpression("(+ 0.25 0.25)").value_number)
	End Method

	Method testSymbolsAreNotCaseSensitive() { test }
		Self.assertEquals(self._lisp.makeSymbol("lower"), self._lisp.makeSymbol("LOWER"))
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Parameter keywords tests
	' ----------------------------------------------------------------------	

	Method testCanDefineFunctionWithoutArgs() { test }
		Self.runExpression("(define (no-arg-test) (+ 1 1))")
		Self.assertEqualsI(2, Self.runExpression("(no-arg-test)").value_number)	
	End Method
			
	Method testCanUseRestKeyword() { test }
		Self.runExpression("(define (rest-test &rest params) (length params))")
		Self.assertEqualsI(3, Self.runExpression("(rest-test 1 2 3)").value_number)
	End Method
	
	Method testCanUserNamedParameters() { test }
		
		Self.runExpression("(define (key-test &key param-1 param-2) (if (> param-1 param-2) 1 2)))")
		Self.assertEqualsI(1, Self.runExpression("(key-test :param-1 10 :param-2 1)").value_number)
		Self.assertEqualsI(2, Self.runExpression("(key-test :param-1 1 :param-2 10)").value_number)
		
	End Method
	
	Method testNamedParametersDefaultToNil() { test }
		
		Self.runExpression("(define (empty-key-test &key param-1) param-1)")
		Self.assertEqualsI(True, Self.runExpression("(empty-key-test)").isNil())
		Self.assertEqualsI(False, Self.runExpression("(empty-key-test :param-1 1)").isNil())		
		
	End Method
	
	Method testNamedParametersCanHaveDefaultValue() { test }
		Self.runExpression("(define (key-defaults-test &key (default-param-1 1) (default-param-2 2)) (if (> default-param-1 default-param-2) 1 2)))")
		Self.assertEqualsI(1, Self.runExpression("(key-defaults-test :default-param-1 10)").value_number)
		Self.assertEqualsI(2, Self.runExpression("(key-defaults-test :default-param-2 10)").value_number)
	End Method
	
	Method testNamedParametersCanHaveComplexDefaultValue() { test }
		Self.runExpression("(define (key-defaults-test &key (default-param-1 (1 2 3))) default-param-1)")
		Local result:LispMax_Atom = Self.runExpression("(key-defaults-test)")
		Self.assertEquals("(1 2 3)", Self._lisp.expressionToString(result))	
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Macro Tests
	' ----------------------------------------------------------------------
	
	Method testCanDefineMacro() { test }
		Self.runExpression("(defmacro (test-macro x) (+ x x))")
		Self.assertEqualsI(4, Self.runExpression("(test-macro 2)").value_number)
	End Method
	
	Method testMacroCanAccessQuasiQuotedArgs() { test }
		Self.runExpression("(defmacro (test-macro x) `(+ ,x ,x))")
		Self.assertEqualsI(4, Self.runExpression("(test-macro 2)").value_number)
	End Method

	
	' ----------------------------------------------------------------------
	' -- Builtin Tests
	' ----------------------------------------------------------------------
	
	Method canAddNumbers() { test }
		Self.assertLispEqualsValue(4, "(+ 1 3)")
		Self.assertLispEqualsValue(10, "(+ 5 5)")
		Self.assertLispEqualsValue(0, "(+ 5 -5)")
	End Method

	Method canSubtractNumbers() { test }
		Self.assertLispEqualsValue(2, "(- 3 1)")
		Self.assertLispEqualsValue(0, "(- 3 3)")
		Self.assertLispEqualsValue(4, "(- 3 -1)")
	End Method

	Method canMultiplyNumbers() { test }
		Local result:Lispmax_Atom = Self.runExpression("(* 10 3)")
		Self.assertEqualsI(30, result.value_number)
	End Method

	Method canDivideNumbers() { test }
		Local result:Lispmax_Atom = Self.runExpression("(/ 10 5)")
		Self.assertEqualsI(2, result.value_number)
	End Method
	
	Method canModulusNumbers() { test }
		self.assertEqualsI(2, Self.runExpression("(mod 10 8)").value_number)
		self.assertEqualsI(0, Self.runExpression("(mod 10 5)").value_number)
	End Method

	Method canRunNestedExpression() { test }
		Local result:Lispmax_Atom = Self.runExpression("(+ (* 2 100) (* 1 10))")
		Self.assertEqualsI(210, result.value_number)
	End Method

	Method canRunBooleanLogic() { test }
		Self.assertEqualsI(2, Self.runExpression("(if (> 6 5) (+ 1 1) (+ 2 2))").value_number)
		Self.assertEqualsI(4, Self.runExpression("(if (< 6 5) (+ 1 1) (+ 2 2))").value_number)
	End Method

	Method canDefineVariables() { test }
		Self.runExpression("(define x 100)")
		Self.assertEqualsI(100, Self.runExpression("x").value_number)
	End Method
	
	Method canDefineLambdas() { test }
		Self.runExpression("(define twice (lambda (x) (* 2 x)))")
		Self.assertEqualsI(10, Self.runExpression("(twice 5)").value_number)
	End Method
	
	Method testEmptyPrognReturnsNil() { test }
		Self.assertEquals(Self._lisp.makeNil(), Self.runExpression("(progn)"))
	End Method
	
	Method testPrognReturnsLastStatementResult() { test }
		Self.assertEqualsI(4, Self.runExpression("(progn (+ 6 5) (+ 1 1) (+ 2 2))").value_number)
	End Method
	
	Method testCanQuoteValues() { test }
		Self.assertEquals("TEST", Self.runExpression("(quote test)").value_symbol)
		Self.assertEquals("TEST", Self.runExpression("'test").value_symbol)
	End Method
	
	Method testCanQuasiQuoteValues() { test }
		Local result:Lispmax_Atom 	= Self.runExpression("`(+ 1 ,(+ 2 3))")
		Self.assertEquals(Self._lisp.expressionToString(result), "(+ 1 5)")	
	End Method

	Method testSymbolPrefixedWithColonReturnsSelf() { test }
		Local symbol:Lispmax_Atom = Self.runExpression(":symbol")
		Self.assertEquals(Self.runExpression(":symbol"), symbol)
	End Method
	
	Method canGetNthValue() { test }
		Self.assertLispEqualsValue(30, "(nth 2 '(10 20 30))")
	End Method

	Method testCanApplyFunctionByName() { test }
		Self.assertLispEqualsValue(3, "(apply + '(1 2))")
	End Method
	
	Method testCanApplyQuotedFunctionName() { test }
		Self.assertLispEqualsValue(3, "(apply '+ '(1 2))")
	End Method

	Method testCanApplyFunctionPointer() { test }
		self.runExpression("(define apply-test-function +)")
		Self.assertLispEqualsValue(3, "(apply apply-test-function '(1 2))")
	End Method

	
	' ----------------------------------------------------------------------
	' -- Library tests
	' ----------------------------------------------------------------------

	Method testLibrarySquareFunction() { test }
		Self.assertLispEqualsValue(9, "(square 3)")
		Self.assertLispEqualsValue(0, "(square 0)")
	End Method
	
	Method testLibraryLengthFunction() { test }
		Self.assertEqualsI(3, Self.runExpression("(length '(1 2 3))").value_number)
		Self.assertLispEqualsValue(3, "(length '(1 2 3))")
	End Method
	
	Method testLibraryNotFunction() { test }
		Self.assertLispTrue("(= 1 1)")
		Self.assertLispNil("(not (= 1 1))")
	End Method
	
	Method testLibraryAndFunction() { test }
		Self.assertLispNil("(and (= 2 1) (= 1 2))")
		Self.assertLispNil("(and (= 1 1) (= 1 2))")
		Self.assertLispNil("(and (= 1 2) (= 2 2))")
		Self.assertLispTrue("(and (= 1 1) (= 2 2))")
	End Method

	Method testLibraryNandFunction() { test }
		Self.assertLispTrue("(nand (= 2 1) (= 1 2))")
		Self.assertLispTrue("(nand (= 1 1) (= 1 2))")
		Self.assertLispTrue("(nand (= 1 2) (= 2 2))")
		Self.assertLispNil("(nand (= 1 1) (= 2 2))")
	End Method
	
	Method testLibraryOrFunction() { test }
		Self.assertLispNil("(or (= 2 1) (= 1 2))")
		Self.assertLispTrue("(or (= 1 1) (= 1 2))")
		Self.assertLispTrue("(or (= 1 2) (= 2 2))")
		Self.assertLispTrue("(or (= 1 1) (= 2 2))")
	End Method

	Method testLibraryAbsFunction() { test }
		Self.assertEqualsI(10, Self.runExpression("(abs (- 0 10))").value_number)
	End Method
	
	Method testLibraryLet() { test }
		Self.assertEqualsI(8, Self.runExpression("(let ((x 3) (y 5)) (+ x y))").value_number)
		'Self.assertEqualsI(0, Self.runExpression("(= (let ((a 42)) a) 42))").value_number)
	End Method
	
	Method testLibraryPositivePredicate() { test }
		Self.assertEquals(self._lisp.makeSymbol("t"), Self.runExpression("(positive? 3)"))	
		Self.assertEquals(self._lisp.makeNil(), Self.runExpression("(positive? -3)"))	
	end Method
	
	Method testLibraryNegativePredicate() { test }
		Self.assertEquals(self._lisp.makeSymbol("t"), Self.runExpression("(negative? -3)"))	
		Self.assertEquals(self._lisp.makeNil(), Self.runExpression("(negative? 3)"))	
	end Method

	Method testLibraryZeroPredicate() { test }
		Self.assertLispNil("(zero? 3)")
		Self.assertLispNil("(zero? -3)")
		Self.assertLispTrue("(zero? 0)")
	end Method

	Method testLibraryOddPredicate() { test }
		Self.assertEquals(self._lisp.makeSymbol("t"), Self.runExpression("(odd? 3)"))	
		Self.assertEquals(self._lisp.makeNil(), Self.runExpression("(odd? 2)"))	
	end Method
	
	Method testLibraryEvenPredicate() { test }
		Self.assertEquals(self._lisp.makeSymbol("t"), Self.runExpression("(even? 2)"))	
		Self.assertEquals(self._lisp.makeNil(), Self.runExpression("(even? 3)"))	
	end Method

	Method testLibraryReduce() { test }
		Self.assertEqualsI(6, Self.runExpression("(reduce + 0 '(1 2 3))").value_number)	
	end Method

	Method testLibrarySum() { test }
		Self.assertEqualsI(6, Self.runExpression("(sum '(1 2 3))").value_number)	
	end Method

'	Method testLibraryMap() { test }
'		Self.assertEqualsI(6, Self.runExpression("(map + 1 2 3)").value_number)
'	end Method

	Method testLibraryMap() { test }
		Self.assertEquals("(nil T nil nil T)", Self._lisp.expressionToString(Self.runExpression("(map 'zero? '(1 0 1 1 0))")))
	End Method

	Method testLibraryFold() { test }
		Self.assertEqualsI(6, Self.runExpression("(foldr + 0 '(1 2 3))").value_number)
		Self.assertEqualsI(6, Self.runExpression("(foldl + 0 '(1 2 3))").value_number)
	end Method

	Method testLibraryWhen() { test }
		Self.assertLispEqualsValue(6, "(when t (+ 3 3))")
		Self.assertLispEqualsValue(6, "(when t (+ 1 1 ) (+ 3 3))")
		Self.assertLispNil("(when nil (+ 1 1 ) (+ 3 3))")
	End Method

	Method testLibraryUnless() { test }
		Self.assertLispEqualsValue(6, "(unless nil (+ 3 3))")
		Self.assertLispEqualsValue(6, "(unless nil (+ 1 1 ) (+ 3 3))")
		Self.assertLispNil("(unless t (+ 1 1 ) (+ 3 3))")
	End Method
	
	Method testLibraryRandomListIndex() { test }
		For Local i:Int = 1 To 10
			Self.assertLispGreaterThanValue(-1, "(random-list-index '(1 2 3))")
			Self.assertLispLessThanValue(3, "(random-list-index '(1 2 3))")
		Next
	End Method

	Method testLibraryRandomListItem() { test }
		For Local i:Int = 1 To 10
			Self.assertLispGreaterThanValue(9, "(random-list-item '(10 11 12))")
			Self.assertLispLessThanValue(13, "(random-list-item '(10 11 12))")
		Next
	End Method

	' ----------------------------------------------------------------------
	' -- Internal stuff
	' ----------------------------------------------------------------------
	
	Method testEnvironmentCanStoreSymbols() { test }
	
		Local symbol:LispMax_Atom = Self._lisp.makeSymbol("TEST")
		Local value:LispMax_Atom  = Self._lisp.MakeString("VALUE")
		Local newValue:LispMax_Atom  = Self._lisp.MakeString("NEW_VALUE")
		
		Self._lisp.setEnvironmentValue(Self._lisp._environment, symbol, value)
		
		Self.assertEquals(value, Self._lisp.getEnvironmentValue(Self._lisp._environment, symbol))
		Self.assertNotSame(newValue, Self._lisp.getEnvironmentValue(Self._lisp._environment, symbol))
		
		Self._lisp.setEnvironmentValue(Self._lisp._environment, symbol, newValue)
		
		Self.assertEquals(newValue, Self._lisp.getEnvironmentValue(Self._lisp._environment, symbol))
		Self.assertNotSame(value, Self._lisp.getEnvironmentValue(Self._lisp._environment, symbol))
		
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
