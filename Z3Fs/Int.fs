module Microsoft.Z3.FSharp.Int

open System
open System.Numerics
open Microsoft.Z3
open Microsoft.Z3.FSharp.Bool

module Z3 = Microsoft.Z3.FSharp.Context

type IntArith = IntExpr of IntExpr
with member x.Expr = match x with IntExpr expr -> expr
     override x.ToString() = match x with IntExpr expr -> sprintf "%O" expr

[<AutoOpen>]
module internal IntUtils =
    let inline mkInt (x: bigint) = Z3.getContext().MkInt(string x)
    let inline add (x: IntExpr) (y: IntExpr) = Z3.getContext().MkAdd(x, y) :?> IntExpr |> IntExpr
    let inline subtract (x: IntExpr) (y: IntExpr) = Z3.getContext().MkSub(x, y) :?> IntExpr |> IntExpr
    let inline multiply (x: IntExpr) (y: IntExpr) = Z3.getContext().MkMul(x, y) :?> IntExpr |> IntExpr
    let inline exp (x: IntExpr) (y: bigint) =
            let rec loop i acc =
                if i = 0I then acc
                else loop (i-1I) (Z3.getContext().MkMul(acc, x))
            if y = 0I then (mkInt 0I) :> IntExpr |> IntExpr
            elif y > 0I then loop (y-1I) x :?> IntExpr |> IntExpr
            else failwith "Coefficient should be a non-negative integer"
    let inline gt (x: IntExpr) (y: IntExpr) = Z3.getContext().MkGt(x, y) |> BoolExpr
    let inline eq (x: IntExpr) (y: IntExpr) = Z3.getContext().MkEq(x, y) |> BoolExpr
    let inline ge (x: IntExpr) (y: IntExpr) = Z3.getContext().MkGe(x, y) |> BoolExpr
    let inline lt (x: IntExpr) (y: IntExpr) = Z3.getContext().MkLt(x, y) |> BoolExpr
    let inline ueq (x: IntExpr) (y: IntExpr) = Z3.getContext().MkDistinct(x, y) |> BoolExpr
    let inline le (x: IntExpr) (y: IntExpr) = Z3.getContext().MkLe(x, y) |> BoolExpr

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

let internal mkIntVar =
    let context = Z3.getContext()
    fun (s: string) -> context.MkIntConst s 

/// Return an int const with supplied name
let Int = mkIntVar >> IntExpr

type Microsoft.Z3.FSharp.Bool.Z3 with
    static member Simplify(IntExpr f, [<ParamArray>] options: (string * _) []) = 
        simplify(f, options) :?> IntExpr |> IntExpr
