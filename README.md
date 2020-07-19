# scala-lisp

A partial implementation of Lisp/Scheme based on [Write You A Scheme 2.0](https://wespiser.com/writings/wyas/00_overview.html) ported from Haskell to Scala using pure functional programming techniques with [cats](https://typelevel.org/cats/), [cats effect](https://typelevel.org/cats-effect/) and [cats monad transformers](https://typelevel.org/cats-mtl/).

It includes an interactive REPL, file system access and its own standard library.

## Running the project

- Install Java 8+ runtime, Scala lang and SBT build tool - [this all can be done through sdk man](https://sdkman.io/)

## CLI options

- start the REPL session:
  ```
  sbt "run repl"
  ...
  λ: (+ 1 2)
  3
  ```
