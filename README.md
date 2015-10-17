# The Luna Programming Language

Luna is a symbolic and rule based programming language - built as a hobby project in an attempt to further
understanding the inner working of other similar languages (such as the Wolfram Language and Prolog). While this
project does not have any ultimate goal, it is in a workable condition, and quite complex programs can be written
from the very small syntax currently available.

# Basic Syntax

Luna in its current state has a fairly limited syntax, although more primitive expressions are being added as
Luna is still early in development.

Integers, Characters, and Lists are all written as follows

    5343 -- Integer
    'x'  -- Character
    {a, b, c} -- List

Basic mathematical operations are built in as operators

    x + y
    x - y
    x * y
    x / y
    x ** y -- Exponentiation

Functions are applied using square braces with commas separating each parameter

    Add[x, y]
    Subtract[x, y]
    Multiply[x, y]
    Divide[x, y]
    Power[x, y]

Rules can be created using the assignment operator

    Fibs[0] := 0
    Fibs[1] := 1
    ...

Pattern matching rules can be used to match arbitrary values

    Add[0, x_] := x -- Underscore means any value (x is the value of x_)
    Add[x_, x_] := Multiply[2, x] -- multiple occurrences of a pattern are required to be equal
    Fibs[n_?IntegerQ] := Fibs[n - 1] + Fibs[n - 2] -- Requires n to satisfy the predicate IntegerQ

# Installation and Usage

Luna includes a cabal build script, to build the interpreter run the following from a terminal:

    $ cabal configure
    $ cabal build

After ensuring that the `lib/Prelude.luna` file is in the same directory as the compiled interpreter, expressions and rules can
be run/created from the command line

    In[] = 1 + 2 + 3
    Out[] = 6
