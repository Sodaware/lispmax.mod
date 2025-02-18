SuperStrict

Framework BaH.MaxUnit
Import sodaware.LispMax

' run the tests!
New TTestSuite.run()

Type Sodaware_LispmaxAtomPool_Tests Extends TTest
	Field _pool:LispMax_AtomPool

	' ----------------------------------------------------------------------
	' -- Create / Delete / Create Again Cycle
	' ----------------------------------------------------------------------

	Method testCreatingAnAtomReturnsPreviouslyDeletedAtom() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		
		' Create an initial atom.
		Local atom:LispMax_Atom = Self._pool.createAtom()

		' Set the atom's value.
		atom.atom_type    = LispMax_Atom.ATOM_TYPE_SYMBOL
		atom.value_symbol = "TEST"

		' Delete the atom.
		Self._pool.destroyAtom(atom)

		' Create another atom.
		Local atom2:LispMax_Atom = Self._pool.createAtom()

		' Check that the atom OBJECT is the same as the previously deleted atom.
		' i.e. check that the atom was reused.
		Self.assertEquals(atom, atom2)

		' Check that the atom's value is NOT the same as the previously set value.
		Self.assertNotSame("TEST", atom2.value_symbol)
	End Method

	' ----------------------------------------------------------------------
	' -- .enableGrowth()
	' ----------------------------------------------------------------------

	Method testEnableGrowthSetsAllowGrowthToTrue() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Self._pool.enableGrowth()
		Self.assertEqualsB(True, Self._pool.allowGrowth)
	End Method

	' ----------------------------------------------------------------------
	' -- .disableGrowth()
	' ----------------------------------------------------------------------

	Method testDisableGrowthSetsAllowGrowthToFalse() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Self._pool.enableGrowth()
		Self._pool.disableGrowth()
		Self.assertEqualsB(False, Self._pool.allowGrowth)
	End Method

	' ----------------------------------------------------------------------
	' -- .getCapacity()
	' ----------------------------------------------------------------------

	Method testGetCapacityReturnsTheCapacityOfThePool() { test }
		Self._pool = LispMax_AtomPool.Create(100)
		Self.assertEqualsI(100, Self._pool.getCapacity())
	End Method

	' ----------------------------------------------------------------------
	' -- .createAtom()
	' ----------------------------------------------------------------------

	Method testCreateAtomReturnsAnAtom() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		
		Self.assertNotNull(Self._pool.createAtom())
	End Method
	
	Method testCreateAtomThrowsAnExceptionIfThePoolIsFullAndGrowthIsDisabled() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Self._pool.createAtom()

		' TODO: Find a better way to test this.
		Try
			Self._pool.createAtom()
			self.assertTrue(False)
		Catch e:LispMax_PoolFullException
			Self.assertTrue(True)
		End Try
	End Method

	Method testCreateAtomGrowsThePoolIfGrowthIsEnabled() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Self._pool.enableGrowth()
		Self._pool.createAtom()
		Self.assertEqualsI(1, Self._pool.getCapacity())
		self._pool.createAtom()
		Self.assertEqualsI(501, Self._pool.getCapacity())
	End Method

	Method testCreateAtomIncreasesTheNumberOfCreatedAtoms() { test }
		Self._pool = LispMax_AtomPool.Create(2)
		Self._pool.createAtom()
		Self.assertEqualsI(1, Self._pool.countCreatedAtoms())
		self._pool.createAtom()
		Self.assertEqualsI(2, Self._pool.countCreatedAtoms())
	End Method

	' ----------------------------------------------------------------------	
	' -- .destroyAtom()
	' ----------------------------------------------------------------------

	Method testDestroyAtomReturnsTheAtomToThePool() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Local atom:LispMax_Atom = Self._pool.createAtom()
		Self._pool.destroyAtom(atom)
		Self.assertEqualsI(1, Self._pool.getCapacity())
	End Method

	Method testDestroyAtomResetsTheAtomToItsInitialState() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Local atom:LispMax_Atom = Self._pool.createAtom()
		
		atom.atom_type    = LispMax_Atom.ATOM_TYPE_SYMBOL
		atom.value_symbol = "TEST"

		Self._pool.destroyAtom(atom)
		
		Self.assertEquals("", atom.value_symbol)	
	End Method
	
	Method testDestroyAtomIncreasesTheNumberOfDeletedAtoms() { test }
		Self._pool = LispMax_AtomPool.Create(1)
		Local atom:LispMax_Atom = Self._pool.createAtom()
		Self.assertEqualsI(0, Self._pool.countDeletedAtoms())
		Self._pool.destroyAtom(atom)
		Self.assertEqualsI(1, Self._pool.countDeletedAtoms())
	End Method

	Method testCanCreatePool() { test }
		Self._pool = LispMax_AtomPool.Create(100)
		Self.assertNotNull(Self._pool)
	End Method

	Method testCanGrowPool() { test }
		Self._pool = LispMax_AtomPool.Create(1)

		self.assertEqualsI(1, Self._pool.getCapacity())

		Self.assertNotNull(Self._pool)
	End Method

End Type