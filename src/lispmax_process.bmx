' ------------------------------------------------------------------------------
' -- lispmax_process.bmx
' --
' -- Wraps a LispMax process. A process can be paused and resumed.
' --
' -- This file is part of lispmax (https://www.sodaware.net/lispmax/)
' -- Copyright (c) 2017-2019 Phil Newton
' --
' -- See COPYING for full license information.
' ------------------------------------------------------------------------------


Type LispMax_Process Extends LispMax

	Global NextProcessId:Int = 1

	Field _pid:Int
	Field _isRunning:Byte  = False
	Field _isFinished:Byte = False
	Field _isSleeping:Byte = False
	Field _sleepUntil:Int  = -1


	' ----------------------------------------------------------------------
	' -- Process Querying
	' ----------------------------------------------------------------------

	''' <summary>Check if the current script process is running.</summary>
	''' <return>True if process is running, false if not.</return>
	Method isRunning:Byte()
		Return Self._isRunning
	End Method

	''' <summary>Check if the current script process has finished.</summary>
	''' <return>True if process has finished, false if not.</return>
	Method isFinished:Byte()
		Return Self._isFinished
	End Method

	''' <summary>Check if the current script process is sleeping.</summary>
	''' <return>True if process is sleeping, false if not.</return>
	Method isSleeping:Byte()
		Return self._isSleeping
	End Method

	''' <summary>Get the process's ID</summary>
	Method getPID:Int()
		Return Self._pid
	End Method


	' ----------------------------------------------------------------------
	' -- Process Execution
	' ----------------------------------------------------------------------

	''' <summary>
	''' Execute the process. Executes any commands on the stack with a maximum
	'''  duration of TIMEOUT milliseconds.
	''' </summary>
	''' <param name="timeout">The maximum number of milliseconds to execute the process.</param>
	Method execute(timeout:Float = 10)

		Local elapsed:Float  = 0
		Local previous:Float = MilliSecs()

		' Check if sleeping
		if self._isSleeping then

			' Do nothing if still sleeping
			if previous < self._sleepUntil then return

			' Unsleep
			self._isSleeping = False
			self._sleepUntil = 0

		End If

		While elapsed < timeout And Self.isRunning() And Not(Self.isFinished()) and not(self.isSleeping())

			' Execute the current frame from the stack
			Self._isFinished = Self.evaluateCurrentExpression()

			elapsed :+ (MilliSecs() - previous)
			previous = MilliSecs()

		Wend

	End Method


	' ----------------------------------------------------------------------
	' -- Process Management
	' ----------------------------------------------------------------------

	' Start everything. Should we initialise the stack here?
	Method startProcess()

		' Mark thread as started
		Self._isRunning = True

	End Method

	Method stopProcess()
		Self._isRunning = False
	End Method

	Method pause()
		Self._isRunning = False
	End Method

	Method resume()
		Self._isRunning = True
	End Method

	''' <summary>Tell the process to sleep for a set number of milliseconds.</summary>
	''' <param name="time">The number of milliseconds to sleep for.</param>
	''' <return>The time at which the process will resume.</return>
	Method sleepFor:int(time:Int)
		Self._isSleeping = true
		self._sleepUntil = MilliSecs() + time
		return self._sleepUntil
	End Method


	' ----------------------------------------------------------------------
	' -- Creation & Initialization
	' ----------------------------------------------------------------------

	Function SpawnProcess:LispMax_Process(parent:Lispmax = Null)

		Local this:LispMax_Process = New LispMax_Process

		If parent <> Null Then
			this._environment   = LispMax_Environment.Create(parent._environment)
			this.symbolTable	= parent.symbolTable
		EndIf

		' Assign a process ID
		this._pid = LispMax_Process.NextProcessId
		LispMax_Process.NextProcessId :+ 1

		Return this

	End Function

End Type
