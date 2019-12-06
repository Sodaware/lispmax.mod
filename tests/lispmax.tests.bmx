SuperStrict

Framework BaH.MaxUnit
Import sodaware.LispMax

Import "lispmax_base_test.bmx"

' run the tests!
New TTestSuite.run()

Type Sodaware_Lispmax_Tests Extends LispMax_BaseTest

	' ----------------------------------------------------------------------
	' -- Atom tests
	' ----------------------------------------------------------------------

	Method testCanUseFloatingPoint() { test }
		Self.assertEqualsF(0.1, Self.runExpression("0.1").value_number)
		Self.assertEqualsF(0.5, Self.runExpression("(+ 0.25 0.25)").value_number)
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

	Method testCanUseNamedParameters() { test }

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
		Local result:Lispmax_Atom   = Self.runExpression("`(+ 1 ,(+ 2 3))")
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

'    Method testCanApplyQuotedFunctionName() { test }
'        Self.assertLispEqualsValue(3, "(apply '+ '(1 2))")
'    End Method

	Method testCanApplyFunctionPointer() { test }
		self.runExpression("(define apply-test-function +)")
		Self.assertLispEqualsValue(3, "(apply apply-test-function '(1 2))")
	End Method


	' ----------------------------------------------------------------------
	' -- String
	' ----------------------------------------------------------------------

	Method testCanConvertStringToUppercase() { test }
		Self.assertLispStringEquals("HELLO, WORLD", "(string-upcase ~qhello, world~q)")
	End Method

	Method testCanConvertStringToLowercase() { test }
		Self.assertLispStringEquals("hello, world", "(string-downcase ~qHELLO, WORLD~q)")
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

End Type
