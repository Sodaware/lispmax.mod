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

	Method testAssocReturnsConsWhenFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertLispExpressionEquals("(~qkey-one~q . ~qvalue-one~q)", "(assoc ~qkey-one~q test-list)")
	End Method

	Method testAssocReturnsNilWhenNotFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertLispNil("(assoc ~qbad-key~q test-list)")
	End Method


	' ----------------------------------------------------------------------
	' -- CDR-ASSOC
	' ----------------------------------------------------------------------

	Method testCdrAssocReturnsConsWhenFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertLispStringEquals("value-one", "(cdr-assoc ~qkey-one~q test-list)")
	End Method

	Method testCdrAssocReturnsNilWhenNotFound() { test }
		Self.runExpression("(define test-list '((~qkey-one~q . ~qvalue-one~q) (~qkey-two~q . ~qvalue-two~q)))")
		Self.assertLispNil("(cdr-assoc ~qbad-key~q test-list)")
	End Method


	' ----------------------------------------------------------------------
	' -- DEFUN
	' ----------------------------------------------------------------------

	Method testDefunCanBeCalledWithSingleArg() { test }
		Self.runExpression("(defun test-function (name) ~qDocstring~q (+ name name))")
		Self.assertLispEqualsValue(20, "(test-function 10)")
	End Method

	Method defunCanBeCalledWithMultipleArgs() { test }
		Self.runExpression("(defun test-function-2 (name other) ~qDocstring~q (+ name other))")
		Self.assertLispEqualsValue(30, "(test-function-2 10 20)")
	End Method

	Method defunCanBeCalledWithoutArgs() { test }
		Self.runExpression("(defun test-function-3 () ~qDocstring~q (+ 1 1))")
		Self.assertLispEqualsValue(2, "(test-function-3)")
	End Method



	' ----------------------------------------------------------------------
	' -- APPEND
	' ----------------------------------------------------------------------

	Method testAppendAddsItemToEndOfList() { test }
		Self.assertLispExpressionEquals("(1 2 3)", "(append (list 1 2) (list 3))")
	End Method

	Method testAppendReturnsNilWhenBothListsEmpty() { test }
		Self.assertLispNil("(append (list) (list))")
	End Method


	' ----------------------------------------------------------------------
	' -- REDUCE
	' ----------------------------------------------------------------------

	Method testCanReduceList() { test }
		Self.assertLispEqualsValue(6, "(reduce + 0 (list 1 2 3))")
	End Method

	Method testCanReduceEmptyList() { test }
		Self.assertLispEqualsValue(0, "(reduce + 0 (list))")
	End Method


	' ----------------------------------------------------------------------
	' -- LET
	' ----------------------------------------------------------------------

	Method testCanLet() { test }
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

	Method testFactorialReturnsCorrectValue() { test }
		Self.assertLispEqualsValue(120, "(! 5)")
	End Method


	' ----------------------------------------------------------------------
	' -- MAP
	' ----------------------------------------------------------------------

'	Method testLibraryMapReturnsProductOfBothLists() { test }
'		Self.assertLispExpressionEquals("(5 7 9)", "(map + '(1 2 3) '(4 5 6))")
'	End Method


	' ----------------------------------------------------------------------
	' -- MAPCAR
	' ----------------------------------------------------------------------

	Method testLibraryMapCarReturnsList() { test }
		Self.assertLispExpressionEquals("(NIL T NIL NIL T)", "(mapcar zero? '(1 0 1 1 0))")
	End Method

	Method testLibraryMapCarAppliesFunctionToEachListItem() { test }
		Self.runExpression("(mapcar assert-called '(1 0 1 1 0))")
		Self.assertLispCalled(5)
	End Method


	' ----------------------------------------------------------------------
	' -- MAPC
	' ----------------------------------------------------------------------

	Method testLibraryMapCAppliesFunctionToEachListItem() { test }
		Self.assertLispExpressionEquals("(1 2 3 4 5)", "(mapc assert-called '(1 2 3 4 5))")
		Self.assertLispCalled(5)
	End Method

	Method testLibraryMapCReturnsOriginalList() { test }
		Self.assertLispExpressionEquals("(1 0 1 1 0)", "(mapc zero? '(1 0 1 1 0))")
	End Method


	' ----------------------------------------------------------------------
	' -- SQUARE
	' ----------------------------------------------------------------------

	Method testLibrarySquareFunction() { test }
		Self.assertLispEqualsValue(9, "(square 3)")
		Self.assertLispEqualsValue(0, "(square 0)")
	End Method


	' ----------------------------------------------------------------------
	' -- LENGTH
	' ----------------------------------------------------------------------

	Method testLibraryLengthFunction() { test }
		Self.assertLispEqualsValue(3, "(length '(1 2 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- NOT
	' ----------------------------------------------------------------------

	Method testLibraryNotFunction() { test }
		Self.assertLispTrue("(= 1 1)")
		Self.assertLispNil("(not (= 1 1))")
		Self.assertLispNil("(not t)")
	End Method


	' ----------------------------------------------------------------------
	' -- AND
	' ----------------------------------------------------------------------

	Method testLibraryAndFunction() { test }
		Self.assertLispNil("(and (= 2 1) (= 1 2))")
		Self.assertLispNil("(and (= 1 1) (= 1 2))")
		Self.assertLispNil("(and (= 1 2) (= 2 2))")
		Self.assertLispNil("(and 2 nil)")
		Self.assertLispTrue("(and (= 1 1) (= 2 2))")
		Self.assertLispTrue("(and t t)")
	End Method


	' ----------------------------------------------------------------------
	' -- NAND
	' ----------------------------------------------------------------------

	Method testLibraryNandFunction() { test }
		Self.assertLispTrue("(nand (= 2 1) (= 1 2))")
		Self.assertLispTrue("(nand (= 1 1) (= 1 2))")
		Self.assertLispTrue("(nand (= 1 2) (= 2 2))")
		Self.assertLispTrue("(nand 2 nil)")
		Self.assertLispNil("(nand (= 1 1) (= 2 2))")
		Self.assertLispNil("(nand t t)")
		Self.assertLispNil("(nand 1 1)")
		Self.assertLispNil("(nand 2 3)")
	End Method


	' ----------------------------------------------------------------------
	' -- OR
	' ----------------------------------------------------------------------

	Method testLibraryOrFunction() { test }
		Self.assertLispNil("(or (= 2 1) (= 1 2))")
		Self.assertLispTrue("(or (= 1 1) (= 1 2))")
		Self.assertLispTrue("(or (= 1 2) (= 2 2))")
		Self.assertLispTrue("(or (= 1 1) (= 2 2))")
	End Method

	Method testOrReturnsFirstNonNilValue() { test }
		Self.assertLispNil("(or nil nil)")
		Self.assertLispTrue("(or nil T)")
		Self.assertLispEqualsValue(5, "(or nil 5)")
		Self.assertLispStringEquals("hello", "(or nil nil nil ~qhello~q ~qgoodbye~q nil)")
	End Method


	' ----------------------------------------------------------------------
	' -- ABS
	' ----------------------------------------------------------------------

	Method testLibraryAbsFunction() { test }
		Self.assertLispEqualsValue(10, "(abs (- 0 10))")
		Self.assertLispEqualsValue(13, "(abs -13)")
	End Method


	' ----------------------------------------------------------------------
	' -- LET
	' ----------------------------------------------------------------------

	Method testLibraryLet() { test }
		Self.assertLispEqualsValue(8, "(let ((x 3) (y 5)) (+ x y))")
	End Method


	' ----------------------------------------------------------------------
	' -- POSITIVE?
	' ----------------------------------------------------------------------

	Method testLibraryPositivePredicate() { test }
		Self.assertLispTrue("(positive? 3)")
		Self.assertLispNil("(positive? -3)")
	end Method


	' ----------------------------------------------------------------------
	' -- NEGATIVE?
	' ----------------------------------------------------------------------

	Method testLibraryNegativePredicate() { test }
		Self.assertLispTrue("(negative? -3)")
		Self.assertLispNil("(negative? 3)")
	end Method


	' ----------------------------------------------------------------------
	' -- ZERO?
	' ----------------------------------------------------------------------

	Method testLibraryZeroPredicate() { test }
		Self.assertLispNil("(zero? 3)")
		Self.assertLispNil("(zero? -3)")
		Self.assertLispTrue("(zero? 0)")
	end Method


	' ----------------------------------------------------------------------
	' -- ODD?
	' ----------------------------------------------------------------------

	Method testLibraryOddPredicate() { test }
		Self.assertLispTrue("(odd? 3)")
		Self.assertLispNil("(odd? 2)")
	end Method


	' ----------------------------------------------------------------------
	' -- EVEN?
	' ----------------------------------------------------------------------

	Method testLibraryEvenPredicate() { test }
		Self.assertLispTrue("(even? 2)")
		Self.assertLispNil("(even? 3)")
	End Method


	' ----------------------------------------------------------------------
	' -- REDUCE
	' ----------------------------------------------------------------------

	Method testLibraryReduce() { test }
		Self.assertLispEqualsValue(6, "(reduce + 0 '(1 2 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- SUM
	' ----------------------------------------------------------------------

	Method testLibrarySum() { test }
		Self.assertLispEqualsValue(6, "(sum '(1 2 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- <=
	' ----------------------------------------------------------------------

	Method testLessThanEqualReturnsCorrectResultWhenTrue() { test }
		Self.assertLispTrue("(<= 10 12)")
		Self.assertLispTrue("(<= 10 10)")
	End Method

	Method testLessThanEqualReturnsCorrectResultWhenFalse() { test }
		Self.assertLispNil("(<= 12 10)")
		Self.assertLispNil("(<= 11 10)")
	End Method


	' ----------------------------------------------------------------------
	' -- >=
	' ----------------------------------------------------------------------

	Method testGreaterThanEqualReturnsCorrectResultWhenTrue() { test }
		Self.assertLispTrue("(>= 12 10)")
		Self.assertLispTrue("(>= 10 10)")
	End Method

	Method testGreaterThanEqualReturnsCorrectResultWhenFalse() { test }
		Self.assertLispNil("(>= 10 12)")
		Self.assertLispNil("(>= 10 11)")
	End Method



	' ----------------------------------------------------------------------
	' -- FOLDR
	' ----------------------------------------------------------------------

	Method testLibraryFoldr() { test }
		Self.assertLispEqualsValue(6, "(foldr + 0 '(1 2 3))")
		Self.assertLispEqualsValue(16, "(foldr + 10 '(1 2 3))")
	End Method

	Method testBuiltinFoldr() { test }
		Self.assertLispEqualsValue(6, "(fast-foldr + 0 '(1 2 3))")
		Self.assertLispEqualsValue(16, "(fast-foldr + 10 '(1 2 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- FOLDL
	' ----------------------------------------------------------------------

	Method testLibraryFoldl() { test }
		Self.assertLispEqualsValue(6, "(foldl + 0 '(1 2 3))")
		Self.assertLispEqualsValue(4, "(foldl - 10 '(1 2 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- WHEN
	' ----------------------------------------------------------------------

	Method testLibraryWhen() { test }
		Self.assertLispEqualsValue(6, "(when t (+ 3 3))")
		Self.assertLispEqualsValue(6, "(when t (+ 1 1 ) (+ 3 3))")
		Self.assertLispNil("(when nil (+ 1 1 ) (+ 3 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- UNLESS
	' ----------------------------------------------------------------------

	Method testLibraryUnless() { test }
		Self.assertLispEqualsValue(6, "(unless nil (+ 3 3))")
		Self.assertLispEqualsValue(6, "(unless nil (+ 1 1 ) (+ 3 3))")
		Self.assertLispNil("(unless t (+ 1 1 ) (+ 3 3))")
	End Method


	' ----------------------------------------------------------------------
	' -- RANDOM-LIST-INDEX
	' ----------------------------------------------------------------------

	Method testLibraryRandomListIndex() { test }
		For Local i:Int = 1 To 10
			Self.assertLispGreaterThanValue(-1, "(random-list-index '(1 2 3))")
			Self.assertLispLessThanValue(3, "(random-list-index '(1 2 3))")
		Next
	End Method


	' ----------------------------------------------------------------------
	' -- RANDOM-LIST-ITEM
	' ----------------------------------------------------------------------

	Method testLibraryRandomListItem() { test }
		For Local i:Int = 1 To 10
			Self.assertLispGreaterThanValue(9, "(random-list-item '(10 11 12))")
			Self.assertLispLessThanValue(13, "(random-list-item '(10 11 12))")
		Next
	End Method

End Type
