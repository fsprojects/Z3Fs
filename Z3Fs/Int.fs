module Microsoft.Z3.FSharp.Int

open System
open System.Numerics

open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type IntArith = IntExpr of IntExpr
with member x.Expr = match x with IntExpr expr -> expr
     override x.ToString() = match x with IntExpr expr -> sprintf "%O" expr

[<AutoOpen>]
module internal IntUtils =
    let inline mkInt (x: bigint) = getContext().MkInt(string x)
    let inline add (x: IntExpr) (y: IntExpr) = getContext().MkAdd(x, y) :?> IntExpr |> IntExpr
    let inline subtract (x: IntExpr) (y: IntExpr) = getContext().MkSub(x, y) :?> IntExpr |> IntExpr
    let inline multiply (x: IntExpr) (y: IntExpr) = getContext().MkMul(x, y) :?> IntExpr |> IntExpr
    let inline divide (x: IntExpr) (y: IntExpr) = getContext().MkDiv(x, y) :?> IntExpr |> IntExpr
    let inline exp (x: IntExpr) (y: bigint) =
            let rec loop i acc =
                if i = 0I then acc
                else loop (i-1I) (getContext().MkMul(acc, x))
            if y = 0I then (mkInt 0I) :> IntExpr |> IntExpr
            elif y > 0I then loop (y-1I) x :?> IntExpr |> IntExpr
            else failwith "Coefficient should be a non-negative integer"
    let inline gt (x: IntExpr) (y: IntExpr) = getContext().MkGt(x, y) |> BoolExpr
    let inline eq (x: IntExpr) (y: IntExpr) = getContext().MkEq(x, y) |> BoolExpr
    let inline ge (x: IntExpr) (y: IntExpr) = getContext().MkGe(x, y) |> BoolExpr
    let inline lt (x: IntExpr) (y: IntExpr) = getContext().MkLt(x, y) |> BoolExpr
    let inline ueq (x: IntExpr) (y: IntExpr) = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le (x: IntExpr) (y: IntExpr) = getContext().MkLe(x, y) |> BoolExpr
    let inline distinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> IntExpr |> IntExpr

type IntArith with
    static member (+)(IntExpr x, IntExpr y) = add x y
    static member (+)(IntExpr x, y: bigint) = add x (mkInt y)
    static member (+)(x: bigint, IntExpr y) = add (mkInt x) y
    static member (+)(x: bigint, y: bigint) = add (mkInt x) (mkInt y)
    static member (-)(IntExpr x, IntExpr y) = subtract x y
    static member (-)(IntExpr x, y: bigint) = subtract x (mkInt y)
    static member (-)(x: bigint, IntExpr y) = subtract (mkInt x) y
    static member (-)(x: bigint, y: bigint) = subtract (mkInt x) (mkInt y)
    static member (*)(IntExpr x, IntExpr y) = multiply x y
    static member (*)(IntExpr x, y: bigint) = multiply x (mkInt y)
    static member (*)(x: bigint, IntExpr y) = multiply (mkInt x) y
    static member (*)(x: bigint, y: bigint) = multiply (mkInt x) (mkInt y) 
    static member (/)(IntExpr x, IntExpr y) = divide x y
    static member (/)(IntExpr x, y: bigint) = divide x (mkInt y)
    static member (/)(x: bigint, IntExpr y) = divide (mkInt x) y
    static member (/)(x: bigint, y: bigint) = divide (mkInt x) (mkInt y)   
    static member Pow(IntExpr x, y: bigint) = exp x y // use this name instead of ( ** )
    static member Pow(x: bigint, y: bigint) = exp (mkInt x) y
    static member (>.)(IntExpr x, IntExpr y) = gt x y
    static member (>.)(IntExpr x, y: bigint) = gt x (mkInt y)
    static member (>.)(x: bigint, IntExpr y) = gt (mkInt x) y
    static member (>.)(x: bigint, y: bigint) = gt (mkInt x) (mkInt y)  
    static member (=.)(IntExpr x, IntExpr y) = eq x y
    static member (=.)(IntExpr x, y: bigint) = eq x (mkInt y)
    static member (=.)(x: bigint, IntExpr y) = eq (mkInt x) y
    static member (=.)(x: bigint, y: bigint) = eq (mkInt x) (mkInt y)  
    static member (>=.)(IntExpr x, IntExpr y) = ge x y
    static member (>=.)(IntExpr x, y: bigint) = ge x (mkInt y)
    static member (>=.)(x: bigint, IntExpr y) = ge (mkInt x) y
    static member (>=.)(x: bigint, y: bigint) = ge (mkInt x) (mkInt y)  
    static member (<.)(IntExpr x, IntExpr y) = lt x y
    static member (<.)(IntExpr x, y: bigint) = lt x (mkInt y)
    static member (<.)(x: bigint, IntExpr y) = lt (mkInt x) y
    static member (<.)(x: bigint, y: bigint) = lt (mkInt x) (mkInt y)  
    static member (<>.)(IntExpr x, IntExpr y) = ueq x y
    static member (<>.)(IntExpr x, y: bigint) = ueq x (mkInt y)
    static member (<>.)(x: bigint, IntExpr y) = ueq (mkInt x) y
    static member (<>.)(x: bigint, y: bigint) = ueq (mkInt x) (mkInt y)  
    static member (<=.)(IntExpr x, IntExpr y) = le x y
    static member (<=.)(IntExpr x, y: bigint) = le x (mkInt y)
    static member (<=.)(x: bigint, IntExpr y) = le (mkInt x) y
    static member (<=.)(x: bigint, y: bigint) = le (mkInt x) (mkInt y)
    static member Distinct xs = Array.map (fun (IntExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, IntExpr expr1, IntExpr expr2) = mkITE b expr1 expr2

let internal mkIntVar =
    let context = getContext()
    fun (s: string) -> context.MkIntConst s 

/// Return an int const with supplied name
let Int = mkIntVar >> IntExpr

type Microsoft.Z3.FSharp.Bool.Z3 with
    static member Simplify(IntExpr f, [<ParamArray>] options: (string * _) []) = 
        simplify(f, options) :?> IntExpr |> IntExpr
