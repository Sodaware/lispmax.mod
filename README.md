# lispmax

> Any sufficiently complicated C or Fortran program contains an ad-hoc,
> informally-specified, bug-ridden, slow implementation of half of Common Lisp.

- *Philip Greenspun*

Now your BlitzMax applications can have one too!

`lispmax` is a module for BlitzMax that can parse and execute a lispy scripting
language.


## A quick example

The following lisp expression adds two numbers together:

```lisp
(+ 10 20)
```

To run this within BlitzMax using lispmax, we need to do the following:

1. Create and initialize a LispMax object - this contains the environment
   (functions, variables etc)
2. Parse the expression - this converts a string containing lisp code into
   something LispMax can use
3. Evaluate the expression - this is where the magic happens

In practice, it looks a little like this:

```blitzmax
Framework brl.basic
Import sodaware.lispmax

' Create a new LispMax instance.
Local lisp:LispMax = New LispMax

' Load built-in and library functions.
lisp.initializeEnvironment()

' Parse and evaluate the expression.
Local expression:LispMax_Atom = lisp.parseExpression("(+ 10 20)")
Local result:LispMax_Atom     = lisp.evaluateExpression(expression)

' Print the result.
lisp.printExpression(result)
```

Running the above example will print `30`.


## Built-in functions

LispMax comes with the following functions built-in:

  - `CAR` - Get the first part of a pair
  - `CDR` - Get the rest of a pair, excluding the CAR
  - `CONS` - Create a new pair
  - `NTH` - Return the nth item from a list
  - `+` - Add
  - `-` - Subtract
  - `*` - Multiply
  - `/` - Divide
  - `MOD` - Modulus
  - `=` - Equal
  - `<` - Less than
  - `>` - Greater than
  - `EQ?` - Check if two atoms are equal.
  - `NIL?` - Predicate to check if an atom is a nil
  - `PAIR?` - Predicate to check if an atom is a pair.
  - `LISTP` - Predicate to check if an atom is a list.
  - `APPLY`
  - `LIST` - Create a list.
  - `OR` - Logical `or`
  - `STRING-UPCASE`- Convert a string to uppercase
  - `STRING-DOWNCASE` - Convert a string to lowercase
  - `RAND` - Generate a random integer
  - `MILLISECS` - Get the number of elapsed milliseconds as an integer
  - `PARSE-INTEGER` - Parse a string into an integer
  - `DEBUGLOG` - Write text to the BlitzMax debuglog
  - `PRINT` - Print a string
  - `PRINT-EXPRESSION` - Convert an expression to a string and print it

Library functions (which are defined in lispmax code, not BlitzMax code) can be
found in `site-lisp/library.lisp`.

Most of these functions behave the same as their Common Lisp counterparts.


## Adding functions

LispMax supports defining functions in BlitzMax, or in lispmax itself.

### Adding native BlitzMax functions

The preferred way to add a native function is to create a type that extends
`LispMax_Callable`.

For example, to add a function that always returns the string "Hello, World":

```blitzmax
Type MyFunction extends LispMax_Callable
    Method call:LispMax_Atom(caller:LispMax, args:LispMax_Atom)
        Return caller.makeString("Hello, World")
    End Method
End Type

lisp.addFunction("my-function", new MyFunction)
```

Native functions are faster than using lispmax code, but they require the
application to be recompiled.

### Adding lispmax functions

Evaluate the following code from within the LispMax environment.

```scheme
(define (my-function)
  "Hello, World")
```

Alternatively, functions can be added using a more lispy syntax:

```lisp
(defun my-function ()
  "Always returns the string Hello World"
  "Hello, World")
```


## Licence

Copyright (C) 2017-2019 Phil Newton

LISPMAX is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

LISPMAX is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with LISPMAX. If not, see http://www.gnu.org/licenses/.
