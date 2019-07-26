# marginal

[Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)) interpeter written in Haskell.

## Building

This uses the `stack` build tool.

```sh
stack setup            # first time only
stack build
```

## Usage

```sh
Marginal: The Whitespace Interpreter

Usage: marginal-exe FILE [-p|--parser [PARSER TYPE]] [--vm [VM]] [-l|--dump-lex]
                    [-i|--dump-instructions]
  Runs whitespace programs - complete with pretty bad error messages

Available options:
  -p,--parser [PARSER TYPE]
                           Parser for instructions (default: "AlexHappy")
  --vm [VM]                Virtual machine to execute
                           instructions (default: "Strict")
  -l,--dump-lex            Dumps the result of lexing
  -i,--dump-instructions   Dumps the parsed instructions
  -h,--help                Show this help text
```
