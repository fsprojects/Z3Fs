module Microsoft.Z3.FSharp.Int

open System
open System.Numerics

open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type Int(expr: IntExpr) = 
    inherit Theory()
    override x.Expr = expr :> Expr
    override x.ToString() = sprintf "%O" expr
    static member FromExpr (e: Expr) = Int(e :?> IntExpr)

let IntExpr expr = Int(expr)
let (|IntExpr|) (i: Int) = i.Expr :?> IntExpr

[<AutoOpen>]
module internal IntUtils =
    let inline mkInt (x: bigint) = getContext().MkInt(string x)
    let inline add x y = getContext().MkAdd(x, y) :?> IntExpr |> IntExpr
    let inline subtract x y = getContext().MkSub(x, y) :?> IntExpr |> IntExpr
    let inline multiply x y = getContext().MkMul(x, y) :?> IntExpr |> IntExpr
    let inline divide x y = getContext().MkDiv(x, y) :?> IntExpr |> IntExpr
    let inline exp x y =
            let rec loop i acc =
                if i = 0I then acc
                else loop (i-1I) (getContext().MkMul(acc, x))
            if y = 0I then (mkInt 0I) :> IntExpr |> IntExpr
            elif y > 0I then loop (y-1I) x :?> IntExpr |> IntExpr
            else failwith "Coefficient should be a non-negative integer"
    let inline gt x y = getContext().MkGt(x, y) |> BoolExpr
    let inline eq x y = getContext().MkEq(x, y) |> BoolExpr
    let inline ge x y = getContext().MkGe(x, y) |> BoolExpr
    let inline lt x y = getContext().MkLt(x, y) |> BoolExpr
    let inline ueq x y = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le x y = getContext().MkLe(x, y) |> BoolExpr
    let inline distinct xs = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> IntExpr |> IntExpr

type Int with
    static member (+)(IntExpr x, IntExpr y) = add x y
    static member (+)(IntExpr x, y) = add x (mkInt y)
    static member (+)(x, IntExpr y) = add (mkInt x) y
    static member (-)(IntExpr x, IntExpr y) = subtract x y
    static member (-)(IntExpr x, y) = subtract x (mkInt y)
    static member (-)(x, IntExpr y) = subtract (mkInt x) y
    static member (*)(IntExpr x, IntExpr y) = multiply x y
    static member (*)(IntExpr x, y) = multiply x (mkInt y)
    static member (*)(x, IntExpr y) = multiply (mkInt x) y
    static member (/)(IntExpr x, IntExpr y) = divide x y
    static member (/)(IntExpr x, y) = divide x (mkInt y)
    static member (/)(x, IntExpr y) = divide (mkInt x) y  
    static member Pow(IntExpr x, y) = exp x y // use this name instead of ( ** )
    static member (>.)(IntExpr x, IntExpr y) = gt x y
    static member (>.)(IntExpr x, y) = gt x (mkInt y)
    static member (>.)(x, IntExpr y) = gt (mkInt x) y
    static member (=.)(IntExpr x, IntExpr y) = eq x y
    static member (=.)(IntExpr x, y) = eq x (mkInt y)
    static member (=.)(x, IntExpr y) = eq (mkInt x) y 
    static member (>=.)(IntExpr x, IntExpr y) = ge x y
    static member (>=.)(IntExpr x, y) = ge x (mkInt y)
    static member (>=.)(x, IntExpr y) = ge (mkInt x) y 
    static member (<.)(IntExpr x, IntExpr y) = lt x y
    static member (<.)(IntExpr x, y) = lt x (mkInt y)
    static member (<.)(x, IntExpr y) = lt (mkInt x) y
    static member (<>.)(IntExpr x, IntExpr y) = ueq x y
    static member (<>.)(IntExpr x, y) = ueq x (mkInt y)
    static member (<>.)(x, IntExpr y) = ueq (mkInt x) y 
    static member (<=.)(IntExpr x, IntExpr y) = le x y
    static member (<=.)(IntExpr x, y) = le x (mkInt y)
    static member (<=.)(x, IntExpr y) = le (mkInt x) y
    static member Distinct xs = Array.map (fun (IntExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, IntExpr expr1, IntExpr expr2) = mkITE b expr1 expr2

/// Return an int const with supplied name
let Int(s: string) = 
    let context = getContext()
    context.MkIntConst s |> IntExpr

let IntVal(i: bigint) = 
    let context = getContext()
    context.MkInt(i.ToString()) :> IntExpr |> IntExpr


