' ------------------------------------------------------------------------------
' -- src/lispmax_atom_map.bmx
' --
' -- A strongly-typed version of the standard BlitzMax map, which cuts down on
' -- the performance overhead of casting between atoms and objects.
' --
' -- Based on the brl.map TMap type.
' --
' -- This file is part of sodaware.mod (https://www.sodaware.net/sodaware.mod/)
' -- Copyright (c) 2009-2017 Phil Newton
' --
' -- See LICENSE for full license information.
' ------------------------------------------------------------------------------


' TODO: Add a "bag" or AtomMap_KeyValue objects so we can quickly list keys?

Private

Global nil:LispMax_AtomMap_Node = New LispMax_AtomMap_Node

nil._color  = LispMax_AtomMap.BLACK
nil._parent = nil
nil._left   = nil
nil._right  = nil

Type LispMax_AtomMap_KeyValue

	Field _key:String
	Field _value:LispMax_Atom

	Method key:String()
		Return self._key
	End Method

	Method value:LispMax_Atom()
		Return self._value
	End Method

End Type

Type LispMax_AtomMap_Node Extends LispMax_AtomMap_KeyValue

	Method nextNode:LispMax_AtomMap_Node()
		Local node:LispMax_AtomMap_Node = Self
		If node._right <> nil Then
			node = _right
			While node._left <> nil
				node = node._left
			Wend
			Return node
		EndIf

		Local parent:LispMax_AtomMap_Node=_parent
		While node = parent._right
			node = parent
			parent = parent._parent
		Wend
		Return parent
	End Method

	Method prevNode:LispMax_AtomMap_Node()
		Local node:LispMax_AtomMap_Node=Self
		If node._left <> nil Then
			node = node._left
			While node._right <> nil
				node = node._right
			Wend
			Return node
		EndIf
		Local parent:LispMax_AtomMap_Node=node._parent
		While node=parent._left
			node=parent
			parent=node._parent
		Wend
		Return parent
	End Method

	Method clear()
		self._parent = Null
		If self._left <> nil Then self._left.clear()
		If self._right <> nil Then self._right.clear()
	End Method

	Method copy:LispMax_AtomMap_Node(parent:LispMax_AtomMap_Node)
		Local t:LispMax_AtomMap_Node = New LispMax_AtomMap_Node
		t._key    = self._key
		t._value  = self._value
		t._color  = self._color
		t._parent = parent

		If _left <> nil then
			t._left=_left.Copy( t )
		EndIf

		If _right <> nil then
			t._right=_right.Copy( t )
		EndIf

		Return t
	End Method

	'***** PRIVATE *****

	Field _color:int
	Field _parent:LispMax_AtomMap_Node = nil
	Field _left:LispMax_AtomMap_Node   = nil
	Field _right:LispMax_AtomMap_Node  = nil

End Type

Public

Type LispMax_AtomMap_NodeEnumerator

	Field _node:LispMax_AtomMap_Node

	Method hasNext:Int()
		Return self._node <> nil
	End Method

	Method nextObject:Object()
		Local node:LispMax_AtomMap_Node = self._node
		self._node = Self._node.nextNode()

		Return node
	End Method

End Type

Type LispMax_AtomMap_KeyEnumerator Extends LispMax_AtomMap_NodeEnumerator
	Method NextObject:Object()
		Local node:LispMax_AtomMap_Node=_node
		_node=_node.NextNode()
		Return node._key
	End Method
End Type

Type LispMax_AtomMap_ValueEnumerator Extends LispMax_AtomMap_NodeEnumerator
	Method NextObject:Object()
		Local node:LispMax_AtomMap_Node=_node
		_node=_node.NextNode()
		Return node._value
	End Method
End Type

Type LispMax_AtomMap_MapEnumerator
	Method ObjectEnumerator:LispMax_AtomMap_NodeEnumerator()
		Return _enumerator
	End Method
	Field _enumerator:LispMax_AtomMap_NodeEnumerator
End Type


Public

Type LispMax_AtomMap

	Field _size:Int = 0

	Method get:LispMax_Atom(key:String)
		Local node:LispMax_AtomMap_Node = Self._findNode(key)

		If node <> nil then Return node._value
	End Method

	Method set(key:String, value:LispMax_Atom)
		Self.Insert(key, value)
	End Method

?Not Threaded
	Method delete()
		self.clear()
	End Method
?

	Method size:Int()
		Return Self._size
	End Method

	Method clear()
		If self._root = nil then Return

		Self._root.clear()
		Self._root = nil
	End Method

	Method isEmpty:Byte()
		Return self._root = nil
	End Method

	Method insert(key:String, value:LispMax_Atom)
		Assert key Else "Can't insert empty key into atom map"

		Local node:LispMax_AtomMap_Node   = _root
		Local parent:LispMax_AtomMap_Node = nil
		Local cmp:Int

		While node <> nil
			parent = node
			cmp    = key.Compare(node._key)

			If cmp > 0 Then
				node = node._right
			ElseIf cmp < 0 Then
				node = node._left
			Else
				node._value = value
				Return
			EndIf
		Wend

		Self._size:+ 1

		node         = New LispMax_AtomMap_Node
		node._key    = key
		node._value  = value
		node._color  = RED
		node._parent = parent

		If parent = nil Then
			Self._root = node
			Return
		EndIf

		If cmp > 0 Then
			parent._right = node
		Else
			parent._left = node
		EndIf

		Self._insertFixup(node)

	End Method

	Method contains:int(key:String)
		Return Self._findNode(key) <> nil
	End Method



	Method valueForKey:LispMax_Atom(key:String)
		Local node:LispMax_AtomMap_Node = self._findNode(key)
		If node <> nil then Return node._value
	End Method

	Method remove:Byte(key:String)
		Local node:LispMax_AtomMap_Node = self._findNode(key)
		If node = nil then Return False

		Self._removeNode(node)
		Self._size:- 1

		Return True
	End Method

	Method keys:LispMax_AtomMap_MapEnumerator()
		Local nodeenum:LispMax_AtomMap_NodeEnumerator = New LispMax_AtomMap_KeyEnumerator
		Local mapenum:LispMax_AtomMap_MapEnumerator   = New LispMax_AtomMap_MapEnumerator

		nodeenum._node      = Self._firstNode()
		mapenum._enumerator = nodeenum

		Return mapenum
	End Method

	Method values:LispMax_AtomMap_MapEnumerator()
		Local nodeenum:LispMax_AtomMap_NodeEnumerator=New LispMax_AtomMap_ValueEnumerator
		nodeenum._node=_FirstNode()
		Local mapenum:LispMax_AtomMap_MapEnumerator=New LispMax_AtomMap_MapEnumerator
		mapenum._enumerator=nodeenum
		Return mapenum
	End Method

	Method copy:LispMax_AtomMap()
		Local map:LispMax_AtomMap=New LispMax_AtomMap
		map._root=_root.Copy( nil )
		Return map
	End Method

	Method objectEnumerator:LispMax_AtomMap_NodeEnumerator()
		Local nodeenum:LispMax_AtomMap_NodeEnumerator=New LispMax_AtomMap_NodeEnumerator
		nodeenum._node=_FirstNode()
		Return nodeenum
	End Method

	'***** PRIVATE *****

	Method _firstNode:LispMax_AtomMap_Node()
		Local node:LispMax_AtomMap_Node=_root
		While node._left<>nil
			node=node._left
		Wend
		Return node
	End Method

	Method _lastNode:LispMax_AtomMap_Node()
		Local node:LispMax_AtomMap_Node = self._root
		While node._right <> nil
			node = node._right
		Wend
		Return node
	End Method

	Method _findNode:LispMax_AtomMap_Node(key:String)
		Local node:LispMax_AtomMap_Node = self._root

		While node <> nil
			Local cmp:int = key.compare(node._key)
			If cmp > 0 Then
				node = node._right
			ElseIf cmp < 0 Then
				node = node._left
			Else
				Return node
			EndIf
		Wend

		Return node
	End Method

	Method _removeNode(node:LispMax_AtomMap_Node)
		Local splice:LispMax_AtomMap_Node
		Local child:LispMax_AtomMap_Node

		If node._left = nil Then
			splice = node
			child  = node._right
		ElseIf node._right = nil Then
			splice = node
			child  = node._left
		Else
			splice = node._left
			While splice._right <> nil
				splice=splice._right
			Wend
			child=splice._left
			node._key=splice._key
			node._value=splice._value
		EndIf
		Local parent:LispMax_AtomMap_Node = splice._parent
		If child<>nil
			child._parent=parent
		EndIf
		If parent=nil
			_root=child
			Return
		EndIf
		If splice=parent._left
			parent._left=child
		Else
			parent._right=child
		EndIf

		If splice._color=BLACK _DeleteFixup child,parent
	End Method

	Method _insertFixup( node:LispMax_AtomMap_Node )

		While node._parent._color = RED And node._parent._parent <> nil
			If node._parent = node._parent._parent._left Then
				Local uncle:LispMax_AtomMap_Node = node._parent._parent._right
				If uncle._color = RED Then
					node._parent._color = BLACK
					uncle._color = BLACK
					uncle._parent._color = RED
					node = uncle._parent
				Else
					If node = node._parent._right Then
						node = node._parent
						self._rotateLeft(node)
					EndIf
					node._parent._color = BLACK
					node._parent._parent._color = RED
					self._rotateRight(node._parent._parent)
				EndIf
			Else
				Local uncle:LispMax_AtomMap_Node = node._parent._parent._left
				If uncle._color = RED Then
					node._parent._color = BLACK
					uncle._color = BLACK
					uncle._parent._color = RED
					node = uncle._parent
				Else
					If node = node._parent._left Then
						node = node._parent
						self._rotateRight(node)
					EndIf
					node._parent._color = BLACK
					node._parent._parent._color = RED
					self._rotateLeft(node._parent._parent)
				EndIf
			EndIf
		Wend
		_root._color=BLACK
	End Method

	Method _rotateLeft(node:LispMax_AtomMap_Node)
		Local child:LispMax_AtomMap_Node = node._right
		node._right = child._left
		If child._left <> nil Then
			child._left._parent = node
		EndIf
		child._parent = node._parent
		If node._parent <> nil Then
			If node = node._parent._left Then
				node._parent._left = child
			Else
				node._parent._right = child
			EndIf
		Else
			_root = child
		EndIf
		child._left=node
		node._parent=child
	End Method

	Method _rotateRight( node:LispMax_AtomMap_Node )
		Local child:LispMax_AtomMap_Node=node._left
		node._left=child._right
		If child._right<>nil
			child._right._parent=node
		EndIf
		child._parent=node._parent
		If node._parent<>nil
			If node=node._parent._right
				node._parent._right=child
			Else
				node._parent._left=child
			EndIf
		Else
			_root=child
		EndIf
		child._right=node
		node._parent=child
	End Method

	Method _deleteFixup(node:LispMax_AtomMap_Node, parent:LispMax_AtomMap_Node)

		While node<>_root And node._color=BLACK
			If node=parent._left

				Local sib:LispMax_AtomMap_Node=parent._right

				If sib._color=RED
					sib._color=BLACK
					parent._color=RED
					_RotateLeft parent
					sib=parent._right
				EndIf

				If sib._left._color=BLACK And sib._right._color=BLACK
					sib._color=RED
					node=parent
					parent=parent._parent
				Else
					If sib._right._color=BLACK
						sib._left._color=BLACK
						sib._color=RED
						_RotateRight sib
						sib=parent._right
					EndIf
					sib._color=parent._color
					parent._color=BLACK
					sib._right._color=BLACK
					_RotateLeft parent
					node=_root
				EndIf
			Else
				Local sib:LispMax_AtomMap_Node=parent._left

				If sib._color=RED
					sib._color=BLACK
					parent._color=RED
					_RotateRight parent
					sib=parent._left
				EndIf

				If sib._right._color=BLACK And sib._left._color=BLACK
					sib._color=RED
					node=parent
					parent=parent._parent
				Else
					If sib._left._color=BLACK
						sib._right._color=BLACK
						sib._color=RED
						_RotateLeft sib
						sib=parent._left
					EndIf
					sib._color=parent._color
					parent._color=BLACK
					sib._left._color=BLACK
					_RotateRight parent
					node=_root
				EndIf
			EndIf
		Wend
		node._color=BLACK
	End Method

	Const RED:int   = -1
	Const BLACK:int = 1

	Field _root:LispMax_AtomMap_Node = nil

End Type
