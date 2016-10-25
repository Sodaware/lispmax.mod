SuperStrict

Framework BaH.MaxUnit
Import sodaware.LispMax

Import "lispmax_base_test.bmx"

' run the tests!
Local suite:TTestSuite = New TTestSuite
suite._addTest(New Sodaware_Lispmax_Library_Tests)
suite.run()

Type Sodaware_Lispmax_Library_Tests Extends LispMax_BaseTest
		
	' ----------------------------------------------------------------------
	' -- ASSOC
	' ----------------------------------------------------------------------
	
	Method assocReturnsConsWhenFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertEquals("(~qkey-one~q . ~qvalue-one~q)", Self._lisp.expressionToString(Self.runExpression("(assoc ~qkey-one~q test-list)")))
	End Method
	
	Method assocReturnsNilWhenNotFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertEquals(LispMax.makeNil(), Self.runExpression("(assoc ~qbad-key~q test-list)"))
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- CDR-ASSOC
	' ----------------------------------------------------------------------
	
	Method cdrAssocReturnsConsWhenFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertLispStringEquals("value-one", "(cdr-assoc ~qkey-one~q test-list)")
	End Method
	
	Method cdrAssocReturnsNilWhenNotFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertEquals(LispMax.makeNil(), Self.runExpression("(cdr-assoc ~qbad-key~q test-list)"))
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- APPEND
	' ----------------------------------------------------------------------
	
	Method appendAddsItemToEndOfList() { test }
		Self.assertEquals("(1 2 3)", Self._lisp.expressionToString(Self.runExpression("(append (list 1 2) (list 3))")))
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- REDUCE
	' ----------------------------------------------------------------------
	
	Method canReduceList() { test }
		Self.assertEqualsi(6, Self.runExpression("(reduce + 0 (list 1 2 3))").value_number)
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- LET
	' ----------------------------------------------------------------------
	
	Method canLet() { test }
		Self.assertLispEqualsValue(100, "(let ((x 1) (y 100)) y)")
	End Method	
	
	
	' ----------------------------------------------------------------------
	' -- COND
	' ----------------------------------------------------------------------
	
	Method testCondEvaluatesCorrectly() { test }
		Self.assertLispEqualsValue(100, "(cond ((= 1 1) 100) ((= 2 1) 200))")
	End Method	
	
	Method testCondReturnsNilWhenNothingMatches() { test }
		Self.assertLispNil("(cond ((= 1 2) 100) ((= 2 1) 200))")
	End Method
	
	
	
	' ----------------------------------------------------------------------
	' -- !
	' ----------------------------------------------------------------------
	
	Method factorialReturnsCorrectValue() { test }
		Self.assertEqualsI(120, Self.runExpression("(! 5)").value_number)
	End Method
		
	Method testLibraryMapCarReturnsList() { test }
        Self.assertEquals("(NIL T NIL NIL T)", Self._lisp.expressionToString(Self.runExpression("(mapcar zero? '(1 0 1 1 0))")))
    End Method
	
	Method testLibraryMapCReturnsOriginalList() { test }
        Self.assertEquals("(1 0 1 1 0)", Self._lisp.expressionToString(Self.runExpression("(mapc zero? '(1 0 1 1 0))")))
    End Method
	
	
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
	
End Type
