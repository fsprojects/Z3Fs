module Microsoft.Z3.FSharp.BitVec

open System
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type BitVecArith = BitVecExpr of BitVecExpr
with member x.Expr = match x with BitVecExpr expr -> expr
     override x.ToString() = match x with BitVecExpr expr -> sprintf "%O" expr

[<AutoOpen>]
module internal IntUtils =
    let inline mkBitVec (v: int) (size: uint32) = getContext().MkBV(v, size)
    let inline add (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVAdd(x, y) |> BitVecExpr
    let inline subtract (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSub(x, y) |> BitVecExpr
    let inline multiply (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVMul(x, y) |> BitVecExpr
    let inline divide (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSDiv(x, y) |> BitVecExpr
    let inline gt (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSGT(x, y) |> BoolExpr
    let inline eq (x: BitVecExpr) (y: BitVecExpr) = getContext().MkEq(x, y) |> BoolExpr
    let inline ge (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSGE(x, y) |> BoolExpr
    let inline lt (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSLT(x, y) |> BoolExpr
    let inline ueq (x: BitVecExpr) (y: BitVecExpr) = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le (x: BitVecExpr) (y: BitVecExpr) = getContext().MkBVSLE(x, y) |> BoolExpr
    let inline distinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> BitVecExpr |> BitVecExpr

type BitVecArith with
    static member (+)(BitVecExpr x, BitVecExpr y) = add x y
    static member (+)(BitVecExpr x, y) = add x (mkBitVec y x.SortSize)
    static member (+)(x, BitVecExpr y) = add (mkBitVec x y.SortSize) y
    static member (+)(x, y) = add (mkBitVec x 32u) (mkBitVec y 32u)
    static member (-)(BitVecExpr x, BitVecExpr y) = subtract x y
    static member (-)(BitVecExpr x, y) = subtract x (mkBitVec y x.SortSize)
    static member (-)(x, BitVecExpr y) = subtract (mkBitVec x y.SortSize) y
    static member (-)(x, y) = subtract (mkBitVec x 32u) (mkBitVec y 32u)
    static member (*)(BitVecExpr x, BitVecExpr y) = multiply x y
    static member (*)(BitVecExpr x, y) = multiply x (mkBitVec y x.SortSize)
    static member (*)(x, BitVecExpr y) = multiply (mkBitVec x y.SortSize) y
    static member (*)(x, y) = multiply (mkBitVec x 32u) (mkBitVec y 32u) 
    static member (/)(BitVecExpr x, BitVecExpr y) = divide x y
    static member (/)(BitVecExpr x, y) = divide x (mkBitVec y x.SortSize)
    static member (/)(x, BitVecExpr y) = divide (mkBitVec x y.SortSize) y
    static member (/)(x, y) = divide (mkBitVec x 32u) (mkBitVec y 32u)   
    static member (>.)(BitVecExpr x, BitVecExpr y) = gt x y
    static member (>.)(BitVecExpr x, y) = gt x (mkBitVec y x.SortSize)
    static member (>.)(x, BitVecExpr y) = gt (mkBitVec x y.SortSize) y
    static member (>.)(x, y) = gt (mkBitVec x 32u) (mkBitVec y 32u)  
    static member (=.)(BitVecExpr x, BitVecExpr y) = eq x y
    static member (=.)(BitVecExpr x, y) = eq x (mkBitVec y x.SortSize)
    static member (=.)(x, BitVecExpr y) = eq (mkBitVec x y.SortSize) y
    static member (=.)(x, y) = eq (mkBitVec x 32u) (mkBitVec y 32u)  
    static member (>=.)(BitVecExpr x, BitVecExpr y) = ge x y
    static member (>=.)(BitVecExpr x, y) = ge x (mkBitVec y x.SortSize)
    static member (>=.)(x, BitVecExpr y) = ge (mkBitVec x y.SortSize) y
    static member (>=.)(x, y) = ge (mkBitVec x 32u) (mkBitVec y 32u)  
    static member (<.)(BitVecExpr x, BitVecExpr y) = lt x y
    static member (<.)(BitVecExpr x, y) = lt x (mkBitVec y x.SortSize)
    static member (<.)(x, BitVecExpr y) = lt (mkBitVec x y.SortSize) y
    static member (<.)(x, y) = lt (mkBitVec x 32u) (mkBitVec y 32u)  
    static member (<>.)(BitVecExpr x, BitVecExpr y) = ueq x y
    static member (<>.)(BitVecExpr x, y) = ueq x (mkBitVec y x.SortSize)
    static member (<>.)(x, BitVecExpr y) = ueq (mkBitVec x y.SortSize) y
    static member (<>.)(x, y) = ueq (mkBitVec x 32u) (mkBitVec y 32u)  
    static member (<=.)(BitVecExpr x, BitVecExpr y) = le x y
    static member (<=.)(BitVecExpr x, y) = le x (mkBitVec y x.SortSize)
    static member (<=.)(x, BitVecExpr y) = le (mkBitVec x y.SortSize) y
    static member (<=.)(x, y) = le (mkBitVec x 32u) (mkBitVec y 32u)
    static member Distinct xs = Array.map (fun (BitVecExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, BitVecExpr expr1, BitVecExpr expr2) = mkITE b expr1 expr2

let BitVec(name: string, size: uint32) =
    let context = getContext()
    context.MkBVConst(name, size) |> BitVecExpr

