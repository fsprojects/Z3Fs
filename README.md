Z3Fs
===
### A simple DSL to solve SMT problems using Z3 API in F# ###

---
### Purpose ###
`Z3Fs` aims at providing a simple API to express and solve SMT problems in F#.
Z3 has an official .NET API written in C# at [Codeplex](http://z3.codeplex.com/SourceControl/changeset/view/89c1785b7322#src/api/dotnet/Context.cs), but it is too verbose and not F#-friendly.
`Z3Fs` is inspired by [Z3Py](http://rise4fun.com/Z3Py) and along the same line of [Scala^Z3](https://github.com/psuter/ScalaZ3) and [SBV](https://github.com/LeventErkok/sbv).
Hopefully, `Z3Fs` could give a look of a small EDSL for SMT solving in F#.

---
### Setup / Installation ###
There are two solutions for Visual Studio 2012 and MonoDevelop 3.0 respectively.
The project currently uses Z3 4.0 64-bit along with F# 3.0. 
Although there are newer versions of Z3, we stick with Z3 4.0 to remedy the problem of setting MonoDevelop project.

---
### Current limitations ###

- Poor support for uninterpreted functions. 
You need explicit type annotations and there is no way to detect inconsistencies between arguments in a type-safe way.
- Limited support for array theory. It suffers from the same symptoms as uninterpreted functions.
- Bad coding habits. The code base currently uses too many type constraints.
- Support for quantifiers would be hard. Maybe it isn't the correct way to go forward.
- The syntax isn't very concise. It might be better to lean towards declarative style.

---
### TODO ###

- Check correctness of examples carefully.
- Implement other theories: Arrays, Datatypes and Quantifiers.
- Rearrange modules; many functions are in wrong places.
- Rewrite ToString() for pretty-printing.
- Standardize the use of %A and %O.
