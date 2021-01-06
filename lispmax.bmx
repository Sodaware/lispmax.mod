' ------------------------------------------------------------------------------
' -- sodaware.lispmax
' --
' -- A terrible Lisp implementation written in BlitzMax. Following along with 
' -- the "Building Lisp" tutorial here: http://www.lwh.jp/lisp/
' --
' -- This file is part of lispmax (https://www.sodaware.net/lispmax/)
' -- Copyright (c) 2017-2019 Phil Newton
' --
' -- This library is free software; you can redistribute it and/or modify
' -- it under the terms of the GNU Lesser General Public License as
' -- published by the Free Software Foundation; either version 3 of the
' -- License, or (at your option) any later version.
' --
' -- This library is distributed in the hope that it will be useful,
' -- but WITHOUT ANY WARRANTY; without even the implied warranty of
' -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' -- GNU Lesser General Public License for more details.
' --
' -- You should have received a copy of the GNU Lesser General Public
' -- License along with this library (see the file COPYING for more
' -- details); If not, see <http://www.gnu.org/licenses/>.
' ------------------------------------------------------------------------------


SuperStrict

Module sodaware.lispmax

' Import core files
Import "src/lispmax_lexer.bmx"
Import "src/lispmax_exceptions.bmx"
Import "src/lispmax_core.bmx"

' Include lisp library code
Incbin "site-lisp/library.lisp"
