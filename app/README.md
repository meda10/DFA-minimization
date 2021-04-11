# FLP - DKA 2 MKA
##### Author: Petr Medek (xmedek07)

## Build and Run
Program is build using command `make`. The project is compiled using the ghc compiler.

```bash
$ ./dka-2-mka (-i|-t|-m) [input-file]
```
* `-i` - prints automaton if it is valid 
* `-t` - prints minimal automaton
* `-m` - prints well-defined automaton (used for testing) 
* `input-file` - name of input file if not defined reeds STDIN

## Description
Program validates automaton, removes unused states and creates minimal deterministic
automaton. Result is printed to STDOUT. Program contains following source files
* `Types.hs` Definitions of data types
* `Main.hs` - Main module, checks arguments and performs IO operations.
* `Minimize.hs` - Module for minimizing automaton 
* `Parse.hs` - Module for parsing input, uses `Text.ParserCombinators.ReadP` library.

## Additional information

* Program expects that automaton **doesn't contain duplicit** states or alphabet letters. 
  If it does, it will consider it as invalid and returns error message
* If automaton doesn't have any transitions, program expects one empty line (`\r` or `\r\n` or `\n`)
  were transitions should be defined, or it will return error message.

## Tests
Directory test contains test files for `dka-2-mka`, tests are written for option `-t` 
* `in` - input files 
* `valid` - expected output files