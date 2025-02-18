' ------------------------------------------------------------------------------
' -- src/lispmax_atompool.bmx
' --
' -- An object pool for LispMax atoms.
' --
' -- This is much faster than using New, but consumes more memory.
' --
' -- This file is part of lispmax (https://www.sodaware.net/lispmax/)
' -- Copyright (c) 2017-2025 Phil Newton
' --
' -- See COPYING for full license information.
' ------------------------------------------------------------------------------


''' <summary>
''' An object pool for LispMax atoms.
'''
''' This is considerably faster than using New, but takes up more memory.
'''
''' By default the pool has a fixed size of 5000 atoms and will raise an exception
''' if it gets full. The initial size can be set at construction, or `enableGrowth`
''' can be called to allow the pool to expand when it becomes full.
'''
''' The pool is not thread safe (yet).
''' </summary>
Type LispMax_AtomPool
	' Constants.
	Const DEFAULT_SIZE:Int    = 5000
	Const DEFAULT_GROW_BY:Int = 500

	' Configuration.
	Field initialSize:Int  = DEFAULT_SIZE
	Field allowGrowth:Byte = False

	' Stats about the pool.
	Field _created:Int = 0
	Field _deleted:Int = 0

	' Internal pool data.
	Field _objects:LispMax_Atom[]
	Field _first:LispMax_Atom

	' ----------------------------------------------------------------------
	' -- Construction / Destruction
	' ----------------------------------------------------------------------

	''' <summary>Create and initalize an atom pool and optionally set the initial size.</summary>
	''' <param name="size">The initial size of the pool.</param>
	''' <returns>The newly created atom pool.</returns>
	Function Create:LispMax_AtomPool(size:Int = 5000)
		Local pool:LispMax_AtomPool = New LispMax_AtomPool

		pool.initialSize = size
		pool._objects = New LispMax_Atom[size]
		pool.init()

		Return pool
	End Function

	' ----------------------------------------------------------------------
	' -- Configuration
	' ----------------------------------------------------------------------

	''' <summary>
	''' Allow the pool to grow if it reaches capacity.
	'''
	''' Resizing the pool is slow, but this option prevents exceptions
	''' from being thrown when the pool is full.
	''' </summary>
	''' <returns>This LispMax_AtomPool.</returns>
	Method enableGrowth:LispMax_AtomPool()
		Self.allowGrowth = True

		Return Self
	End Method

	''' <summary>Disable growth of the pool.</summary>
	''' <returns>This LispMax_AtomPool.</returns>
	Method disableGrowth:LispMax_AtomPool()
		Self.allowGrowth = False

		Return Self
	End Method

	' ----------------------------------------------------------------------
	' -- Pool Information
	' ----------------------------------------------------------------------

	''' <summary>Get the capacity of the pool.</summary>
	''' <returns>The capacity of the pool.</returns>
	Method getCapacity:Int()
		Return Self._objects.Length
	End Method

	''' <summary>
	''' Get the total number of atoms created using `createAtom()`.
	'''
	''' This is NOT the number of atoms currently in the pool or the number
	''' of atoms currently in use. It's just the total number of times that
	''' `createAtom()` has been called.
	''' </summary>
	Method countCreatedAtoms:Int()
		Return Self._created
	End Method

	''' <summary>
	''' Get the total number of atoms destroyed using `destroyAtom()`.
	''' </summary>
	''' <returns>The total number of atoms destroyed.</returns>
	Method countDeletedAtoms:Int()
		Return Self._deleted
	End Method

	' ----------------------------------------------------------------------
	' -- Creating / Destroying Atoms
	' ----------------------------------------------------------------------

	''' <summary>Fetch a new atom from the pool.</summary>
	''' <returns>A new LispMax_Atom.</returns>
	Method createAtom:LispMax_Atom()
		' Check if the pool is full. Resize or throw an exception.
		If Self._first = Null Then
			If Self.allowGrowth Then
				Self.grow(500)
			else
				Throw LispMax_PoolFullException.Create(Self)
			End If
		End If

		Self._created :+ 1

		' Fetch the next available atom.
		Local obj:LispMax_Atom = Self._first
		Self._first = obj._next

		Return obj
	End Method

	''' <summary>Destroy an atom and return it to the pool.</summary>
	''' <param name="obj">The atom to destroy.</param>
	''' <returns>This LispMax_AtomPool.</returns>
	Method destroyAtom:LispMax_AtomPool(obj:LispMax_Atom)
		' Reset the atom to its initial state.
		obj.reset()

		' Update internal state.
		Self._deleted :+ 1

		' Add back the pool.
		obj._next = Self._First
		Self._first = obj

		Return Self
	End Method

	' ----------------------------------------------------------------------
	' -- Pool Management
	' ----------------------------------------------------------------------

	''' <summary>Initialize the atom pool.</summary>
	Method init()
		' Create all atoms required for the pool.
		For Local i:Int = 0 To Self.initialSize - 1
			Local o:LispMax_Atom = New LispMax_Atom
			Self._objects[i]     = o
		Next

		' Reset internal pointers.
		Self.reset()
	End Method

	''' <summary>Reset the pool to its initial state.</summary>
	Method reset()
		For Local i:Int = 0 To Self.initialSize - 2
			Self._objects[i]._next = Self._objects[i + 1]
		Next

		Self._first = Self._objects[0]
	End Method

	''' <summary>Grow the pool by a given number of atoms.</summary>
	''' <param name="growBy">The number of atoms to grow by.</param>
	''' <returns>This LispMax_AtomPool.</returns>
	Method grow:LispMax_AtomPool(growBy:Int = DEFAULT_GROW_BY)
		' Resize the pool.
		Self._objects = Self._objects[..Self.initialSize + growBy]

		' Create the new atoms.
		For Local i:Int = Self.initialSize To Self.initialSize + growBy - 1
			Local o:LispMax_Atom = New LispMax_Atom
			Self._objects[i]     = o
		Next

		' Mark the newest atom as the first.
		Self._first = Self._objects[Self.initialSize - 1]

		' Update the size.
		Self.initialSize :+ growBy

		Return Self
	End Method
End Type

''' <summary>Exception thrown when the atom pool is full.</summary>
Type LispMax_PoolFullException Extends LispMax_Exception
	Field _pool:LispMax_AtomPool

	Function Create:LispMax_PoolFullException(pool:LispMax_AtomPool)
		Local this:LispMax_PoolFullException = New LispMax_PoolFullException
		this._pool = pool
		Return this
	End Function

	Method ToString:String()
		Return "The LispMax pool is full. Size: " + Self._pool.initialSize + ". Created: " + Self._pool._created + ". Deleted: " + self._pool._deleted
	End Method
End Type
