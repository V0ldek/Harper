# Harper

Hybrid language allowing for both imperative and functional programming

## Type system

The basis of the type system is a segregation into two categories of data types:  **value** and **ref** types, as well as **pure**, **impure** and **sideeffect** functions. 

Value types are immutable and behave as in Haskell or other functional languages.

Ref types are like reference types in C# (or Java) -- they are mutable and there may be many references to the same object.

Pure, impure and sideeffect categories seek to clearly distinguish between parts of the program that are independent and testable and the ones that handle state. For example, it allows users to expose a pure public API which asserts that it always produces the same results for the same arguments, but use mutable state as its implementation detail.

The problem is that functional programmers view an expression `f x` differently than imperative programmers. In a functional context, it's just partial application. In an imperative context it's a call that may result in side effects. Therefore, Harper's type system is forced to distinguish between
```
a -> b -> c
```
and
```
a -> (b -> c)
```
For example, consider a value type `Map a b` that allows for addition and lookup of items of an arbitrary type. Fix that type to be `Map String (Integer -> Integer -> Integer)`. Now consider a function:
```
printAndGet map x = {
  val f = map.lookup x;
  call (print f 42 42);
  return f;
}
```

In a pure context, we expect that something like this:

```
g = printAndGet map "Str" 42;
```
will yield `g :: Integer -> Integer` with `42` partially applied to whatever `map.lookup "Str"`would evaluate to. We expect that to be computed lazily and result in no side effects. But in a `sideeffect` context, a statement
```
g = printAndGet map "Str";
```
 is expected to actually print the result of `f 42 42`. In conclusion, `impure` and `sideffect` functions must distinguish between the formal parameters of themselves and the objects they return. Therefore, `printAndGet` in Harper has a type of
 ```
 printAndGet :: Map String (Integer -> Integer -> Integer) -> String -> sideeffect -> Integer -> Integer -> Integer
 ```
From this we infer:
- a **pure** function is one that does not take `impure` or `sideeffect` as its first parameter
- an **impure** function is one that has `impure` as its first parameter
- a **sideeffect** function is one that has `sideeffect` as its first parameter.

The `impure` and `sideeffect` are special types of parameters. The only value that can be used where `impure` or `sideeffect` is required is the unit `()`. All functions taking ref types are automatically `impure`, since the argument might be mutated between calls. A pure function may still create and manipulate ref types and call `impure` functions, with the caveat that it makes the sequencing of operations within the functions body important. It doesn't break the contract of pure functions, since Harper doesn't allow for global state, so indeed calling a function without any ref types as arguments may be considered as if it ran within an isolated environment. However, if a `sideeffect` is invoked, the entire function becomes `sideeffect`.
```
printAndGet map :: String -> sideeffect -> Integer -> Integer -> Integer
printAndGet map "Str" :: sideeffect -> Integer -> Integer -> Integer
printAndGet map "Str" () :: Integer -> Integer -> Integer
// But the evaluation of the above expression causes something to be printed onscreen.
```
So, a function that handles and evaluates values `printAndGet`, `printAndGet map` and `printAndGet map "Str"` may still be considered pure. But the moment it consumes the side effects by stating `printAndGet map "Str" ()` it becomes `sideeffect` itself.

Note that this allows to store values of type `sideeffect` and `impure` to be executed later.


## Other features

### Locals with `val`, `var`
 Two types of local variables - `val` and `var` (akin to Scala), `val` is not reassignable, `var` is. 

### Member functions

Basically a syntactic sugar that allows to call a function `f :: T -> a` on `x :: t` using `x.f`. One can also access the function itself with `T.f`. For variant types, functions are declared per-variant. For example, only `List.Nonempty` has `head` defined, so calling `head` on `List.Empty` throws an error.

### Generators

A  special `Generator a` polymorphic type that gets returned if a function contains a `yield` statement. Ref types return an equivalent ref type `RefGenerator a`.

### Pattern-based `Iterable`

Any type declaring a member function `iterate :: t -> Generator a` is considered a subtype of `Iterable a`. This is the only place in the language where "subtype" is a concept. When saying `for val x in t` one constrains the `t` to be a subtype of `Iterable a`. This instruction calls the `iterate` function to get a generator and uses it to iterate over the structure. `Generator a` is also a subtype of `Iterable a `. The same applies to `RefGenerator a` and `RefIterable a`.

This introduces a caveat to the Member Functions feature. To be considered an instance of `Iterable`, `iterate` must be declared in _all_ of the type's variants. If that were not the case, say `List.Nonempty` declares `iterate` but `List.Empty`does not and we consider `List` an instance of `Iterable`, then it'd be possible to pass an empty `List a` as an `Iterable a` and then get an execution-time error that'd be impossible to prevent on the callee's side.

### Ad-hoc types

Ability to generate simple dictionary-like values in any place in the code. For example `x = new { a = 1; b = "str"; }` creates a value of type `{ a :: Integer, b :: String }`.

### Pattern matching

Ability to pattern-match literals, tuples, data types based on their data (including ad-hoc types). Can be done with a special `match x {}` instruction or directly in a variable declaration, including lambda parameters. Throws an error on a failed match.

## Examples

See `lang/Examples` folder.

## Formal grammar

See `lang/Harper.cf`.
