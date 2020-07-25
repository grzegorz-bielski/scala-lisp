# scala-lisp

An (outstandingly) partial implementation of Lisp/Scheme based on [Write You A Scheme 2.0](https://wespiser.com/writings/wyas/00_overview.html) ported from Haskell to Scala using pure functional programming techniques with [cats](https://typelevel.org/cats/), [cats effect](https://typelevel.org/cats-effect/) and [cats monad transformers](https://typelevel.org/cats-mtl/).

It includes an interactive REPL, file system access, and its own, although very poor, standard library.

## Running the project

- Install Java 8+ runtime, Scala lang and SBT build tool - [this all can be done through sdk man](https://sdkman.io/)

## CLI options

- interpret the file at given path:

  ```bash
  sbt "run path <path>"
  ```

  ```bash
   sbt "run path ./src/main/resources/demo/add.scm"
   ...
   3
  ```

- start the REPL session:
  ```bash
  sbt "run repl"
  ...
  λ: (+ 1 2)
  3
  ```
- eval passed expression:
  ```bash
  sbt 'run eval "(+ 1 2)"'
  ...
  3
  ```

## todo

- variable declaration in REPL (!)
- Fix IO bugs, make it more robust
- rewrite to StateT, implement Y combinator
