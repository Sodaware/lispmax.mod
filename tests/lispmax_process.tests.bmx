SuperStrict

Framework BaH.MaxUnit
Import sodaware.LispMax

' run the tests!
New TTestSuite.run()

Type Sodaware_LispmaxProcess_Tests Extends TTest
	
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
	' -- Builtin Tests
	' ----------------------------------------------------------------------
	
	Method testCanCreateProcess() { test }
		Self.assertNotNull(LispMax_Process.SpawnProcess(Self._lisp))
	End Method
	
	Method testProcessCanEvaluateExpression() { test }
		
		Local expression:LispMax_Atom = Self._lisp.parseExpression("(+ 1 2)")
		Local process:LispMax_Process = Self._lisp.createProcess(expression)
		
		' Run the process
		process.startProcess()
		process.execute()
		
		Self.assertEqualsI(3, process.getResult().value_number)
	
	End Method
	
	Method testCanPauseExecution() { test }
		
		Self.runExpression("(define (pause-test) (+ 1 1) (suspend) (+ 3 3))")
		
		local process:LispMax_Process = Self._lisp.createProcess(Self._lisp.parseExpression("(pause-test)"))

		' Run the process
		process.startProcess()
		process.execute(1000)
		
		' Result should be first 
		Self.assertEqualsI(2, process.getResult().value_number)
	
	End Method
	
	Method testCanResumeExecution() { test }
		
		Self.runExpression("(define (pause-test) (+ 1 1) (suspend) (+ 3 3))")
		
		local process:LispMax_Process = Self._lisp.createProcess(Self._lisp.parseExpression("(pause-test)"))

		' Run the process
		process.startProcess()
		process.execute(1000)
		
		' Result should be first 
		Self.assertEqualsI(2, process.getResult().value_number)
		
		process.resume()
		process.execute()

		Self.assertEqualsI(6, process.getResult().value_number)
	
	End Method
	
	
	
	
	
	' ----------------------------------------------------------------------
	' -- Helpers
	' ----------------------------------------------------------------------
	
	Method runExpression:Lispmax_Atom(content:String)
	
		Local expression:LispMax_Atom = Self._lisp.parseExpression(content)
		Return Self._lisp.evaluateExpression(expression, Self._lisp._environment)
	
	End Method
		
	
End Type
