## Banana Repl (with Scala)
This is a Scala implementation of the `Banana` toy language parser, desugarer, and interpreter originally implemented in `Racket/Plait`

### Usage

##### Compiling

```sh
scala-cli --power package --assembly -o BananaRepl .
```

You'll likely need to install `scala`, `scala-cli`, and their respective dependencies if you want to make changes and recompile the source code.
The easiest way is to follow the `cs setup` instructions [here](https://www.scala-lang.org/download/)

##### Running the Executable

```sh
$ ./BananaRepl
```
The project & all dependencies are bundled in an executable fat jar `BananaRepl`
On running the program, you'll be prompted to choose to either:

  * start a REPL session that will let you enter expressions and evaluate them
  * enter a file path and evaluate the expression included in the file (useful for evaluating multi-line or more complicated expressions)

### Making Changes & Recompiling (must have Scala & scala-cli installed)
##### Why did you choose the language you did?
Scala provides first-class support for many of the language features of Plait that were incredibly
valuable in making the original parser/desugarer/interpreter implementation:
  * Algebraic Data Types
  * Pattern Matching
  * Permissive/expressive syntax for identifier names

Some of the additional strengths of Scala that I felt would make it a strong candidate for re-implementing the parser/desugarer/interpreter include:
  * Nearly all data structures from the standard library are functors/monads
    * I made extensive use of map/fold/flatMap on the Option and Either types in my implementation
  * The [Typelevel](https://typelevel.org) ecosystem provides some really cool libraries for pure FP
    * I used the [cats-parse](https://github.com/typelevel/cats-parse) library for parsing
      * Being able to compose parsers from combinators using infix operators made the parser really expressive and powerful
      * I was able to parse a `SurfaceExpr` from the raw string directly without needing to first parse an `S-Exp`
  * Scala 3's `extension` methods make it really easy to add methods to objects that implicitly call other functions

##### What, if anything, was more difficult in your chosen language than in Plait?
The main difficulty I encountered was that because Scala doesn't have built-in support for s-expressions, my parser implementation had to do the additional
work of parsing a string to an s-expression while parsing to a surface-expression AST. `cats-parse` proved to be an invaluable resource for that.


##### Do your implementations have the same behavior?
I (for the most part) created a faithful implementation of the same behavior as the original, but with some differences.
  * This parser may be more forgiving about omitting whitespace between bracketed terms since it knows identifiers will never have parentheses or brackets in them
  * Because lexing tokens and parsing are done in one step, the parser also accepts language-reserved keywords as valid identifiers if they appear in identifier position
  * Numbers are implemented as 64-bit precision floating point decimals.
    * This is perhaps not as rich as the number implementation provided by Plait, and there may be some subtle differences caused by floating point arithmetic.
  * Error handling is done at the return-level and does not rely on throwing/catching exceptions.
    * All steps that can fail due to bad input (parse, interpret) return values wrapped in an `Either` type so that errors have to be explicitly handled by the caller
  * The values returned by the interpreter are largely the same as in the Plait implementation, but the string representations printed in the console are meant to mimic JS/Python syntax


##### To the extent that you assert that your implementations have the same behavior, how do you know?
Scala's strong type system helps to make some guarantees about the behavior of this program,
but I also have some automated tests that can be run within scala-cli
```sh
scala-cli test .
```
