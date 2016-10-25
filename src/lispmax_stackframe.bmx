' ------------------------------------------------------------------------------
' -- src/lispmax_stackframe.bmx
' -- 
' -- Stack frames represent a single level of the execution stack.
' ------------------------------------------------------------------------------


Type LispMax_StackFrame
	
	Field _parent:LispMax_StackFrame		'< Stack that called this stack
	Field _environment:Lispmax_Environment	'< Environment at time of push
	Field _op:LispMax_Atom					'< Evaluated operator
	Field _tail:LispMax_Atom				'< Args not yet evaluated
	Field _args:LispMax_Atom				'< Args evaluated
	Field _body:LispMax_Atom				'< Expressions that will eventually be evaluated
	
	
	' ----------------------------------------------------------------------
	' -- Configuration Helpers
	' ----------------------------------------------------------------------
	
	Method setOperator:LispMax_StackFrame(op:LispMax_Atom)
		Self._op = op
		Return Self
	End Method
	
	Method setArgs:LispMax_StackFrame(args:LispMax_Atom)
		Self._args = args
		Return Self
	End Method
	
	Method setTail:LispMax_StackFrame(tail:LispMax_Atom)
		Self._tail = tail
		Return Self
	End Method
	
	Method setBody:LispMax_StackFrame(body:LispMax_Atom)
		Self._body = body
		Return Self
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Getting Information
	' ----------------------------------------------------------------------
	
	Method getStackEnvironment:Lispmax_Environment()
		Return Self._environment
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Execution
	' ----------------------------------------------------------------------
		
	''
	' Called once an expression has been evaluated. Stores the result, either as
	' an operator, argument or body expression. Fetches the next expression to 
	' evaluate.
	Method doReturn:LispMax_Atom(expression:LispMax_Atom, environment:LispMax_Environment)
		
		' Still running a procedure, ignore the result
		If Not(Self._body.isNil()) Then
			Return Self.doApply(expression, environment)
		EndIf
		
		' [todo] - Can this be removed?
		If Self._op.isNil() Then
				
			' Finished
		
			
		
		End If
		
	End Method
	
	' [todo] - Can this be removed?
	Method doApply:LispMax_Atom(expression:LispMax_Atom, environment:LispMax_Environment)
		
	End Method
	
	Method dump(indent:String)
		If Self._parent <> Null Then
			Self._parent.dump(indent + "  ")
		Else
			DebugLog indent + " -> " + Self.ToString()
		End If
	End Method
	
	
	' ----------------------------------------------------------------------
	' -- Construction
	' ----------------------------------------------------------------------
	
	Function Create:LispMax_StackFrame(parent:LispMax_StackFrame, environment:LispMax_Environment, tail:LispMax_Atom)
		Local this:LispMax_StackFrame = New LispMax_StackFrame
		
		this._parent		= parent
		this._environment	= environment
		this._tail 			= tail
		
		this._op			= LispMax.makeNil()
		this._args			= LispMax.makeNil()
		this._body			= LispMax.makeNil()
		
		Return this
	End Function
	
End Type