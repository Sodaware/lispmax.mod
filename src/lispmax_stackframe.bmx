' ------------------------------------------------------------------------------
' -- src/lispmax_stackframe.bmx
' --
' -- Stack frames represent a single level of the execution stack.
' --
' -- This file is part of sodaware.mod (https://www.sodaware.net/sodaware.mod/)
' -- Copyright (c) 2009-2017 Phil Newton
' --
' -- See LICENSE for full license information.
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
	' -- Debugging
	' ----------------------------------------------------------------------

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
		this._tail			= tail

		this._op			= LispMax.makeNil()
		this._args			= LispMax.makeNil()
		this._body			= LispMax.makeNil()

		Return this
	End Function

End Type
