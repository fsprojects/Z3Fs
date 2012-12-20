Z3Fs
===
### A simple DSL to solve SMT problems using Z3 API in F# ###

---
### Purpose ###
`Z3Fs` aims at providing a simple API to express and solve SMT problems in F#.
Z3 has an official .NET API written in C# at [Codeplex](http://z3.codeplex.com/SourceControl/changeset/view/89c1785b7322#src/api/dotnet/Context.cs), but it is too verbose and not F#-friendly.
Hopefully, `Z3Fs` could give a look of a small EDSL for SMT solving in F#.

---
### Setup / Installation ###

---
### Implementation notes ###

---
### TODO ###

- Check correctness of examples carefully.
- Examine difficult modules including uninterpreted functions and bit-vectors.
- Rearrange modules; many functions are in wrong places.
- Rewrite ToString() for pretty-printing.
- Standardize the use of %A and %O.
