module Microsoft.Z3.Array

open System
open Microsoft.Z3
open Microsoft.Z3.Bool

type Array<'I, 'O>(expr: ArrayExpr) = 
  inherit Theory()
  override x.Expr = expr :> Expr
  override x.ToString() = sprintf "%O" expr
  static member FromExpr (e: Expr) = Array<_, _>(e :?> ArrayExpr)
  static member (=.)(a1: Array<'I, 'O>, a2: Array<'I, 'O>) =
      Gs.context().MkEq(a1.Expr, a2.Expr) |> BoolExpr
  static member (<>.)(a1: Array<'I, 'O>, a2: Array<'I, 'O>) =
      Gs.context().MkDistinct(a1.Expr, a2.Expr) |> BoolExpr

let ArrayExpr<'I, 'O> expr = Array<'I, 'O>(expr)
let (|ArrayExpr|) (a: Array<_, _>) = a.Expr :?> ArrayExpr

let inline Array<'I, 'O when 'I :> Theory and 'O :> Theory and 'O: (static member FromExpr : Expr -> 'O)>
        (s: string, domain: Sort, range: Sort) = 
    let context = Gs.context()
    context.MkArrayConst(s, domain, range) |> ArrayExpr<'I, 'O>

let inline Select<'I, ^O  when 'I :> Theory and ^O: (static member FromExpr : Expr -> ^O)> 
        (a: Array<'I, ^O>) (i: 'I) =
    let expr = Gs.context().MkSelect(a.Expr :?> ArrayExpr, i.Expr)
    (^O : (static member FromExpr : Expr -> ^O) expr)

let inline Store<'I, 'O  when 'I :> Theory and 'O :> Theory> 
        (a: Array<'I, 'O>) (i: 'I) (x: 'O) =
    Gs.context().MkStore(a.Expr :?> ArrayExpr, i.Expr, x.Expr) |> ArrayExpr<'I, 'O>
