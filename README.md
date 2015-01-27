FsZ3
====

A simple DSL to solve SMT problems using Z3 API in F#.

``` bash
git clone https://github.com/haf/FsZ3.git
bundle
bundle exec rake
```

### Purpose

[Z3](https://github.com/Z3Prover/z3) is a high-performance theorem prover being
developed at Microsoft Research. Z3 has an official .NET API written in C#, but
it is too verbose and not F#-friendly. This project aims at providing a simple
API to:

 - do constraint solving,
 - check satisfiability of logical formulas, and
 - prove theorems,

in a concise way in F#.

FsZ3 is inspired by [Z3Py](http://rise4fun.com/Z3Py) and along the same line of
[Scala^Z3](https://github.com/psuter/ScalaZ3) and
[SBV](https://github.com/LeventErkok/sbv).

### Examples

For instance, here is a very simple constraint solving problem in FsZ3:

```fsharp
// Create 3 integer variables
let dog = Int("dog")
let cat = Int("cat")
let mouse = Int("mouse")

Z3.Solve(dog >=. 1I,   // at least one dog
         cat >=. 1I,   // at least one cat
         mouse >=. 1I, // at least one mouse
         // we want to buy 100 animals
         dog + cat + mouse =. 100I,
         // We have 100 dollars (10000 cents):
         // dogs cost 15 dollars (1500 cents),
         //   cats cost 1 dollar (100 cents), and
         //   mice cost 25 cents
         1500I * dog + 100I * cat + 25I * mouse =. 10000I)
```

Take a look at [Examples](Examples) for an extensive set of examples.

### Setup / Installation

 - 64 bit mono
 - [albacore](https://github.com/albacore/albacore)
 - Run `bundle exec rake` and check out `./src/Examples`

### Current limitations

- Poor support for uninterpreted functions.  You need explicit type annotations
  and there is no way to detect inconsistencies between arguments in a type-safe
  way.
- Limited support for array theory. It suffers from the same symptoms as
  uninterpreted functions.
- Bad coding habits. The code base currently uses too many type constraints.
- Support for quantifiers would be hard. Maybe it isn't the correct way to go
  forward.
- The syntax isn't very concise. It might be better to lean towards declarative
  style.

### TODO

- Check correctness of examples carefully.
- Implement other theories: Arrays, Datatypes and Quantifiers.
- Rearrange modules; many functions are in wrong places.
- Rewrite ToString() for pretty-printing.
- Standardize the use of %A and %O.

### License

[The MIT license](LICENSE.txt)
