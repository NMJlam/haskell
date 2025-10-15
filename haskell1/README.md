---
title: Week 6 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

- [Requirements](#requirements)
- [Using the code bundle](#using-the-code-bundle)
  - [Example of testing your code](#example-of-testing-your-code)

## Requirements

For installation instructions, see the [Haskell Installation section of the unit notes](https://tgdwyer.github.io/haskell0/).

Pretty please complete all the exercises. There must be no compilation warnings or errors.

Recommended order:

1. `BinTree.hs`
2. `NaryTree.hs`
3. `MappingMaybe.hs`
4. `Examples.hs`

## Using the code bundle

Once you have downloaded and extracted the code, set it up by running:

```bash
$ stack setup
```

- You should only have to do this once throughout the semester

Build your code files by executing

```bash
$ stack build
```

To run, debug, or test the code, enter Haskell's interactive environment,
GHCi[^6]:

```bash
$ stack ghci
```

This will be the only way to test your code this week, so make sure you are comfortable with
this terminal

It should load all the source files in the current project. If you
modify your code and want to test it, you need to `:load/:l` or `:reload/:r`
your code, the former will load the file you indicate, while the latter will
reload the whole project:

[^6]:
    GHCi is a REPL (Read-Eval-Print-Loop), which means that whatever code you
    type in it is interpreted as if it were source code; loading a file is
    equivalent to copy/pasting it.

```bash
*Main Lib> :l src/BinTree.hs
[1 of 1] Compiling BinTree           ( src/BinTree.hs, interpreted )
Ok, modules loaded: BinTree
*BinTree> :r
[1 of 1] Compiling BinTree           ( src/BinTree.hs, interpreted )
Ok, modules loaded: BinTree.
```

The interactive environment gives you a number of tools to explore the code,
typing `:type/:t` will give you the type of a function; to have more
information, you can use `:info/:i`.

```bash
*Maybes> :t mapTree
mapTree :: (Int -> Int) -> BinTree -> BinTree
*Maybes> :i mapTree
mapTree :: (Int -> Int) -> BinTree -> BinTree
  	-- Defined at src/BinTree.hs:82:1
```

Using GHCi makes debugging faster as each time you reload your file(s) it will
inform you of any syntax or type error.

You can also type code directly within GHCi and use its results either by
copy/pasting into your code, or using `it` to recall the last results.

```bash
Prelude> map (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
Prelude> map (*2) it
[4,6,8,10,12,14,16,18,20,22]
```

### Example of testing your code

Running `stack ghci` should open up the interactive GHCi console.

```bash
stack ghci
```

```bash
ghci> :l src/BinTree.hs
[1 of 1] Compiling BinTree          ( src/BinTree.hs, interpreted )
Ok, one module loaded.
```

The code has successfully compiled. We can now test a function, e.g., `size`.
However, we have not completed this exercise, so it returns an undefined.

```bash
ghci> size tree
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at src/BinTree.hs:34:8 in main:BinTree
```

After completing the exercise

```bash
ghci> :r
[1 of 1] Compiling BinTree          ( src/BinTree.hs, interpreted )
Ok, one module loaded.
ghci> size tree
4
```

This matches the expected value documented in BinTree, so our answers is most likely correct.
