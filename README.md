# Harper

Hybrid language allowing for both imperative and functional programming, featuring a strong, static type system enabling the programmer to handle side effects in a safe and flexible manner.

The basis of the type system is a segregation into two categories of data types:  **value** and **ref** types, as well as **pure**, **impure** and **sideeffect** functions. 

Value types are immutable and behave as in Haskell or other functional languages.

Ref types are like reference types in C# (or Java) -- they are mutable and there may be many references to the same object.

Pure, impure and sideeffect categories seek to clearly distinguish between parts of the program that are independent and testable and the ones that handle state. For example, it allows users to expose a pure public API which asserts that it always produces the same results for the same arguments, but use mutable state as its implementation detail.

For additional details see the Spec document `lang/Spec.md`.

Examples of Harper programs can be found in the `lang/Examples` folder.

Formal grammar is located in `lang/Harper.cf`.

## Build and run

The interpreter is written in Haskell and uses stack for the build pipeline. Use `stack run harper` to build and run or `stack install` to install the `harper` executable. Tests can be run with `stack test`.