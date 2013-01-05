module Microsoft.Z3.FSharp.Array

open System
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type Array<'I, 'O>(expr: ArrayExpr) = 
    inherit Theory()
    override x.Expr = expr :> Expr
    override x.ToString() = sprintf "%O" expr
    static member FromExpr (e: Expr) = Array<_, _>(e :?> ArrayExpr)

let ArrayExpr<'I, 'O> expr = Array<'I, 'O>(expr)
let (|ArrayExpr|) (a: Array<_, _>) = a.Expr :?> ArrayExpr

let inline Array<'I, 'O when 'I :> Theory and 'O :> Theory and 'O: (static member FromExpr : Expr -> 'O)>
        (s: string, domain: Sort, range: Sort) = 
    let context = getContext()
    context.MkArrayConst(s, domain, range) |> ArrayExpr<'I, 'O>

let inline Select<'I, ^O  when 'I :> Theory and ^O: (static member FromExpr : Expr -> ^O)> 
        (a: Array<'I, ^O>) (i: 'I) =
    let expr = getContext().MkSelect(a.Expr :?> ArrayExpr, i.Expr)
    (^O : (static member FromExpr : Expr -> ^O) expr)

let inline Store<'I, 'O  when 'I :> Theory and 'O :> Theory> 
        (a: Array<'I, 'O>) (i: 'I) (x: 'O) =
    getContext().MkStore(a.Expr :?> ArrayExpr, i.Expr, x.Expr) |> ArrayExpr<'I, 'O>
