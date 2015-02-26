#!/bin/bash

# Usage:
#   bash run-tests.sh
# 
# Run tests and redirect outputs to files 'res-x.txt' where 'x' corresponds to the task numbers.

DIR=tests
TEST=../$DIR

# Create results directory, if it do not already exist
mkdir -p $DIR

cd src

# The naive monad for nondeterminism
## Task 1
# TODO: use 'open Nondet.Naive' in 'Puzzle.ml'
#ocamlbuild Puzzle.native
#./Puzzle.native > $TEST/res-1.txt

# Choices trees
## Task 2.1
# No result
## Task 2.2
ocamlbuild Bools.native
./Bools.native > $TEST/test-2-2.txt
ocamlbuild Puzzle.native
./Puzzle.native >> $TEST/test-2-2.txt
## Task 2.3
ocamlbuild Sumless.native
./Sumless.native > $TEST/test-2-3.txt
ocamlbuild Append.native
./Append.native >> $TEST/test-2-3.txt

# A taste of Prolog
## Task 3.1 and Task 3.2
ocamlbuild Typing.native
./Typing.native > $TEST/test-3.txt

# Probabilistic programming
## Task 4.1
ocamlbuild Probatests.native
./Probatests.native > $TEST/test-4-1.txt
## Task 4.2
ocamlbuild Cluedo.native
./Cluedo.native > $TEST/test-4-2.txt

# Imperative nondeterministic programming
## Task 5
ocamlbuild Sumless_imp.native
./Sumless_imp.native > $TEST/test-5.txt

# Monadic data structures and memoization
## Task 6.1 and 6.3
ocamlbuild Mlist.native
./Mlist.native > $TEST/test-6-1-6-3.txt
## Task 6.2
ocamlbuild Testmemo.native
./Testmemo.native > $TEST/test-6-2.txt
## Task 6.4
ocamlbuild Mtyping.native
./Mtyping.native > $TEST/test-6-4.txt

cd ..
