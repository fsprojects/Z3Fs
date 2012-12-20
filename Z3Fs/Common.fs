module Microsoft.Z3.FSharp.Common

open System.Collections.Generic
open Microsoft.Z3

let mutable private context = new Context(Dictionary())
let getContext() = context
let setContext c = context <- c
    
let Solver() = getContext().MkSolver()

let inline (++) xs ys = Array.append xs ys

/// Implement IEnumerable interface to support for..in..do construct 
type Microsoft.Z3.Statistics with
    member x.GetEnumerator() =
        (Seq.map (fun k -> k, x.[k]) x.Keys).GetEnumerator()

/// Multiple indexers for evaluating formulas
type Microsoft.Z3.Model with
    member x.Item 
        with get (index: Expr) = x.Eval(index, true)    
    member x.Item 
        with get (index: FuncDecl) = x.ConstInterp(index)   