# Harper

## 1. Introduction

Harper is a hybrid functional and imperative language with a robust type system that makes side-effects explicit. 

Harper is functional at its roots, with out-of-the-box currying, lazy evaluation, lambda expressions, polymorphic data types and pattern matching. Harper guarantees purity of computations unless the user explicitly opts out.

Imperative style of programming is allowed within a function's body, including variables, loops and `return` statements. This allows the programmer to write code that is simple to design and understand using those imperative constructs while retaining the benefits of a functional, pure type system.

The type system includes user-defined value types, which are Harper's implementation of variant data types (a.k.a. type unions), as well as ref types, similar to reference types in C# (or all non-primitive types in Java). The main feature of Harper are the two special types `impure` and `sideeffect`. Those types make any impurities introduced by imperative programming techniques that cause side effects visible in other places in code explicit in the function signature. When a signature contains neither of those special types the programmer can be sure that the function in question is pure.

Harper acknowledges that many programming problems require operations on collections of data and provides support for generator functions and iterable sequences with the `for in` loop and `Iterable a`, `RefIterable a b` types.

### Hello World

```
main :: sideeffect -> ();
main = {
  eval printLn "Hello, World!" ();
};
```

This classical example illustrates a few key points of Harper's design.
- The `main` function's type means "this function produces a side effect and returns the Unit value (intuitively, returns nothing)".
- Expressions used only for their side effects have to be preceded by `eval`.
- `printLn` is a built-in function of type `a -> sideeffect -> ()`.
- Function application is done by simply putting the argument expression after the function expression separated by whitespace.
- `"Hello, World!"` is of type `String` and is applied to the `printLn` function. It matches the generic type variable `a` and results in a `sideeffect -> ()` function (partial application).
- The unit literal `()` is used to explicitly consume the `sideeffect` part of the result.

Harper programs have a `.har` extension by convention. Assuming the above code is located in `hello.har` one can execute it with:
```
harper hello.har
```
The result will be:
```
Hello, World!  
  
Execution ended with value:  
()
```
First, all output of the program being run is printed, and then the value returned by the `main` function is given.

### Program structure

An element appearing on the top level of a program may be one of the following:
- type definition
- function type hint
- function definition

The interpreter expects a function named `main` with a signature matching
`*`, `impure -> *`, `sideeffect -> *`, where `*` can be any type. If it is not present, then execution ends with an error. The program is fully statically analysed before that.

### Primitive types

Harper defines a number of primitive types that cannot be defined directly in Harper.


|Type|Description |
|--|--|
|`()`| Unit type, only the value `()` is of this type.  |
|`Integer` | Integer value without size restrictions.
|`Bool` | Boolean `true` or `false` value.
|`Char` | Unicode character.
|`String`|Sequence of unicode characters.
| a `->` b | Function type.

## 2. Type system

The Harper's defining feature is its purity based type system. Every expression in the language has a statically determined type. Harper is strongly typed, i.e. the user has no way of circumventing the type system's decisions by casting or otherwise.

### Pure types

The base of the type system are expressions that are pure. The subset of Harper that does not influence purity consists of all expressions composed of pure parts including lambdas, as well as local values, which are immutable. User-defined value types are also pure. Limited to this subset, Harper behaves as most functional languages.
- Every object is either a function or a value.
- Functions are effectively single-parameter and return an object.
- Expressions are lazily evaluated -- the values are not computed before they are needed.

The typing rules are simplified when compared to a language like Haskell. First of all, every top level function, member function, data member, local value or a variable have a type specified by the user in form of a type hint. There is no type reconstruction.
```
f :: a -> b; // Type hint of a function f.
```
That means that types of any receivers are always statically known, i.e. function arguments, identifiers that can be assigned to, etc. always know what type they expect to be passed into them. Typing then boils down to deciding whether the type of an expression matches the expected type.

First some notation. Uppercase identifiers denote type names and can be followed by a number of type arguments. A lowercase identifier other than `impure` and `sideeffect` denotes an unbound type variable, e.g. `a`. A lowercase identifier followed by `&` denotes a bound type variable, e.g. `a&`. The same identifier is either bound or unbound, never both.

The difference between unbound and bound type variables is that an unbound type variable is universally quantified, i.e. it can be replaced by any type. A bound type variable is a type variable that was replaced by a particular, concrete type, but it is unknown what type it is. For example, when `f :: a -> a`, then within the body of `f` `a&` is bound - it is the concrete type of the first argument passed to `f`, but since it is generic we do not know what type exactly it is. However, it is not interchangeable with other types.

The following rules apply:
- Any type matches an unbound type variable.
- No type matches a bound type variable except for itself.
- A value or ref type `T a1 ... an` matches `T b1 ... bn` iff for every $1 \leq i \leq n$ `ai` matches `bi`. This includes tuple types `(a1 ... an)`.
- A function `p1 -> r1` matches `p2 -> r2` iff `p1` matches `p2` and `r1` matches `r2`.
- Any primitive type `T` matches itself.

The rules governing binding of type variables are as follows:
- Type variables are unbound by default.
- All type variables specified in a function's type hint are bound within that functions body.
- Type variables in type hints of local values and variables are immediately bound.
- Type variables acting as type parameters of a user-defined type are bound within that type's body.

Example illustrating all these cases:

```
value T a = {
  // a is bound to a& here.
  data = {
    fldA :: a;
    fldF :: a -> b; // b is unbound.
  };
};

fun :: a -> b;
fun x = {
  // Both a and b are bound to a& and b&.
  var (x :: c); // c is bound to c&.
  return T.fldF; // fldF is of type a -> b for unbound a and b,
				    so it matches a& -> b&.
};
```

### Impurities

There are two special types, `impure` and `sideeffect`, that separate pure parts of Harper code from the impure ones.

Let us start with `sideeffect`, as it is the simpler to explain. A function that has `sideeffect` as part of its type produces input/output side effects. Currently the only way to introduce `sideeffect` to user code is to use `print` or `printLn` built-in functions. Any function that would otherwise have a type `a -> b` for any `a` and `b` that produces side effects by evaluating an expression that produces side effects becomes `a -> sideeffect -> b`. The only value of type `sideeffect` is the unit literal `()`.

```
f :: Integer -> sideeffect -> Integer;
// f is a function that takes an Integer and produces an Integer,
// but it also prints to screen, so it gets "tainted" with a sideeffect.
f n = {
  eval printLn "Message." ();
  return n;
};
```

As seen above, the `eval` statement can be used to evaluate an expression solely for its impure effects.

The `impure` type is different. I/O side effects bubble up into the signatures of all functions using them. On the other hand, `impure` results from the imperative portions of `Harper`, but they can be enclosed as implementation details invisible to other users. Consider a simple reference type `Ref a` that provides a layer of indirection for the type `a` and can be mutated. It defines functions `get` and `set` . Those functions are impure. The setter mutates the structure which can be visible in other places that hold a reference to it. The getter's result depends on the previous set operations.

However, if a function creates its own instance of `Ref a`, performs computations using it and returns a result of a pure type it still may be pure. Notice that as long as the function's parameter types are all pure and the return type is pure, the function itself is also pure. Therefore impure computations do not cause a bubble up like side effects do. Closing over variables or accepting or returning reference types or other impure types does.

The rules are as follows:
- `impure` type is naturally impure.
- Any reference type is impure.
- A value type whose any type argument is impure is also impure.
- A function whose parameter or return type is impure is also impure.
- A local function (a lambda expression) that closes over a local variable or a ref local value is impure.

To see why the last point is important consider:

```
var (y :: Integer) = 42;
(f :: <type1>) = \(x :: Integer) => x + y;
(n1 :: <type2>) = f 42;
y := 0;
(n2 :: <type3>) = f 42;
```
To maintain type safety `n1` necessarily needs to equal `n2`. But an imperative programmer would expect `f` to close over the variable `y` and reflect changes done to it. This is indeed a useful technique, so `f` must be impure and `type1 = Integer -> impure -> Integer`, `type2 = impure -> Integer`. Both `n1` and `n2` now contain equivalent functions. If instead we consumed the impurity by calling `f 42 ()` then `n1` would be `82` and `n2` would be `42`.

Additional rules are needed for typing of impure types:
- A unit literal `()` matches `impure`.
- `impure` matches `sideeffect`
- `impure` and `sideeffect` do match an unbound type variable.
- A type `a` for any `a` matches both `impure -> a` and `sideeffect -> a`.

The last rule allows the user to intentionally pass a pure value into an impure context. Impure types are more general than pure ones.

Additional rules stem from the semantics of iterator functions and are covered in the next section.

## 3. Iterator functions

