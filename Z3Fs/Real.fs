module Microsoft.Z3.FSharp.Real

open System
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type RealArith = RealExpr of RealExpr
with member x.Expr = match x with RealExpr expr -> expr
     override x.ToString() = match x with RealExpr expr -> sprintf "%O" expr

[<AutoOpen>]
module internal RealUtils =
    let inline mkReal (x: float) = getContext().MkReal(string x)
    let inline add (x: RealExpr) (y: RealExpr) = getContext().MkAdd(x, y) :?> RealExpr |> RealExpr
    let inline subtract (x: RealExpr) (y: RealExpr) = getContext().MkSub(x, y) :?> RealExpr |> RealExpr
    let inline multiply (x: RealExpr) (y: RealExpr) = getContext().MkMul(x, y) :?> RealExpr |> RealExpr
    let inline divide (x: RealExpr) (y: RealExpr) = getContext().MkDiv(x, y) :?> RealExpr |> RealExpr
    let inline exp (x: RealExpr) y =
            let rec loop i acc =
                if i = 0I then acc
                else loop (i-1I) (getContext().MkMul(acc, x))
            if y = 0I then (mkReal 0.) :> RealExpr |> RealExpr
            elif y > 0I then loop (y-1I) x :?> RealExpr |> RealExpr
            else failwith "Coefficient should be a non-negative integer"
    let inline gt (x: RealExpr) (y: RealExpr) = getContext().MkGt(x, y) |> BoolExpr
    let inline eq (x: RealExpr) (y: RealExpr) = getContext().MkEq(x, y) |> BoolExpr
    let inline ge (x: RealExpr) (y: RealExpr) = getContext().MkGe(x, y) |> BoolExpr
    let inline lt (x: RealExpr) (y: RealExpr) = getContext().MkLt(x, y) |> BoolExpr
    let inline ueq (x: RealExpr) (y: RealExpr) = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le (x: RealExpr) (y: RealExpr) = getContext().MkLe(x, y) |> BoolExpr
    let inline distinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> RealExpr |> RealExpr

type RealArith with
    static member (+)(RealExpr x, RealExpr y) = add x y
    static member (+)(RealExpr x, y) = add x (mkReal y)
    static member (+)(x, RealExpr y) = add (mkReal x) y
    static member (+)(x, y) = add (mkReal x) (mkReal y)
    static member (-)(RealExpr x, RealExpr y) = subtract x y
    static member (-)(RealExpr x, y) = subtract x (mkReal y)
    static member (-)(x, RealExpr y) = subtract (mkReal x) y
    static member (-)(x, y) = subtract (mkReal x) (mkReal y)
    static member (*)(RealExpr x, RealExpr y) = multiply x y
    static member (*)(RealExpr x, y) = multiply x (mkReal y)
    static member (*)(x, RealExpr y) = multiply (mkReal x) y
    static member (*)(x, y) = multiply (mkReal x) (mkReal y)  
    static member (/)(RealExpr x, RealExpr y) = divide x y
    static member (/)(RealExpr x, y) = divide x (mkReal y)
    static member (/)(x, RealExpr y) = divide (mkReal x) y
    static member (/)(x, y) = divide (mkReal x) (mkReal y)  
    static member Pow(RealExpr x, y) = exp x y
    static member Pow(x, y) = exp (mkReal x) y
    static member (>.)(RealExpr x, RealExpr y) = gt x y
    static member (>.)(RealExpr x, y) = gt x (mkReal y)
    static member (>.)(x, RealExpr y) = gt (mkReal x) y
    static member (>.)(x, y) = gt (mkReal x) (mkReal y)  
    static member (=.)(RealExpr x, RealExpr y) = eq x y
    static member (=.)(RealExpr x, y) = eq x (mkReal y)
    static member (=.)(x, RealExpr y) = eq (mkReal x) y
    static member (=.)(x, y) = eq (mkReal x) (mkReal y)  
    static member (>=.)(RealExpr x, RealExpr y) = ge x y
    static member (>=.)(RealExpr x, y) = ge x (mkReal y)
    static member (>=.)(x, RealExpr y) = ge (mkReal x) y
    static member (>=.)(x, y) = ge (mkReal x) (mkReal y)  
    static member (<.)(RealExpr x, RealExpr y) = lt x y
    static member (<.)(RealExpr x, y) = lt x (mkReal y)
    static member (<.)(x, RealExpr y) = lt (mkReal x) y
    static member (<.)(x, y) = lt (mkReal x) (mkReal y)  
    static member (<>.)(RealExpr x, RealExpr y) = ueq x y
    static member (<>.)(RealExpr x, y) = ueq x (mkReal y)
    static member (<>.)(x, RealExpr y) = ueq (mkReal x) y
    static member (<>.)(x, y) = ueq (mkReal x) (mkReal y)  
    static member (<=.)(RealExpr x, RealExpr y) = le x y
    static member (<=.)(RealExpr x, y) = le x (mkReal y)
    static member (<=.)(x, RealExpr y) = le (mkReal x) y
    static member (<=.)(x, y) = le (mkReal x) (mkReal y)
    static member Distinct xs = Array.map (fun (RealExpr expr) -> expr :> Expr) xs |> distinct
    static member If(BoolExpr b, RealExpr expr1, RealExpr expr2) = mkITE b expr1 expr2

/// Return a real const with supplied name
let Real(s: string) =
    let context = getContext()
    context.MkRealConst s |> RealExpr

type Microsoft.Z3.FSharp.Bool.Z3 with
    static member Simplify(RealExpr f, [<ParamArray>] options: (string * _) []) = 
        simplify(f, options) :?> RealExpr |> RealExpr