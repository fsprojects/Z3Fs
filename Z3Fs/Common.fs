module Microsoft.Z3.FSharp.Common

open System.Collections.Generic
open Microsoft.Z3

let mutable private context = new Context(Dictionary())
let getContext() = context
    
let Solver() = getContext().MkSolver()

let inline (++) xs ys = Array.append xs ys

[<AbstractClass>]
type Theory() = 
    abstract member Expr: Expr

/// Implement IEnumerable interface to support for..in..do construct 
type Microsoft.Z3.Statistics with
    member x.GetEnumerator() =
        (Seq.map (fun k -> k, x.[k]) x.Keys).GetEnumerator()

type Result =
    | Const of Expr
    | Func of FuncInterp
with override x.ToString() =
        match x with
        | Const expr -> sprintf "%O" expr
        | Func f -> sprintf "%O" f

/// Multiple indexers for evaluating formulas
type Microsoft.Z3.Model with
    member x.Item (index: Expr) = x.Eval(index, true)    
    member x.Item (index: FuncDecl) = 
        if index.Arity <> 0u then x.FuncInterp(index) |> Func
        else x.ConstInterp(index) |> Const
    member x.Evaluate(v: Theory, ?completion) =
            x.Evaluate(v.Expr, defaultArg completion false)