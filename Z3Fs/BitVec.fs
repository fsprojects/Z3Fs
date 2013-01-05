module Microsoft.Z3.FSharp.BitVec

open System
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type BitVec(expr: BitVecExpr) = 
    inherit Theory()
    override x.Expr = expr :> Expr
    override x.ToString() = sprintf "%O" expr
    static member FromExpr (e: Expr) = BitVec(e :?> BitVecExpr)

let BitVecExpr expr = BitVec(expr)
let (|BitVecExpr|) (bv: BitVec) = bv.Expr :?> BitVecExpr

[<AutoOpen>]
module internal IntUtils =
    let inline mkBitVec (v: int) (size: uint32) = getContext().MkBV(v, size)
    let inline add x y = getContext().MkBVAdd(x, y) |> BitVecExpr
    let inline subtract x y = getContext().MkBVSub(x, y) |> BitVecExpr
    let inline multiply x y = getContext().MkBVMul(x, y) |> BitVecExpr
    let inline sdiv x y = getContext().MkBVSDiv(x, y) |> BitVecExpr
    let inline smod x y = getContext().MkBVSMod(x, y) |> BitVecExpr
    let inline shl x y = getContext().MkBVSHL(x, y) |> BitVecExpr
    let inline ashr x y = getContext().MkBVASHR(x, y) |> BitVecExpr
    let inline bvand x y = getContext().MkBVAND(x, y) |> BitVecExpr
    let inline bvor x y = getContext().MkBVOR(x, y) |> BitVecExpr
    let inline bvnot x = getContext().MkBVNeg(x) |> BitVecExpr
    let inline gt x y = getContext().MkBVSGT(x, y) |> BoolExpr
    let inline eq x y = getContext().MkEq(x, y) |> BoolExpr
    let inline ge x y = getContext().MkBVSGE(x, y) |> BoolExpr
    let inline lt x y = getContext().MkBVSLT(x, y) |> BoolExpr
    let inline ueq x y = getContext().MkDistinct(x, y) |> BoolExpr
    let inline le x y = getContext().MkBVSLE(x, y) |> BoolExpr
    let inline distinct xs = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> BitVecExpr |> BitVecExpr
    let inline ult x  y = getContext().MkBVULT(x, y) |> BoolExpr
    let inline ule x y = getContext().MkBVULT(x, y) |> BoolExpr
    let inline ugt x y = getContext().MkBVULT(x, y) |> BoolExpr
    let inline uge x y = getContext().MkBVULT(x, y) |> BoolExpr
    let inline udiv x y = getContext().MkBVUDiv(x, y) |> BitVecExpr
    let inline umod x y = getContext().MkBVURem(x, y) |> BitVecExpr
    let inline lshr x y = getContext().MkBVLSHR(x, y) |> BitVecExpr

type BitVec with
    static member (+)(BitVecExpr x, BitVecExpr y) = add x y
    static member (+)(BitVecExpr x, y) = add x (mkBitVec y x.SortSize)
    static member (+)(x, BitVecExpr y) = add (mkBitVec x y.SortSize) y
    static member (-)(BitVecExpr x, BitVecExpr y) = subtract x y
    static member (-)(BitVecExpr x, y) = subtract x (mkBitVec y x.SortSize)
    static member (-)(x, BitVecExpr y) = subtract (mkBitVec x y.SortSize) y
    static member (*)(BitVecExpr x, BitVecExpr y) = multiply x y
    static member (*)(BitVecExpr x, y) = multiply x (mkBitVec y x.SortSize)
    static member (*)(x, BitVecExpr y) = multiply (mkBitVec x y.SortSize) y
    static member (/)(BitVecExpr x, BitVecExpr y) = sdiv x y
    static member (/)(BitVecExpr x, y) = sdiv x (mkBitVec y x.SortSize)
    static member (/)(x, BitVecExpr y) = sdiv (mkBitVec x y.SortSize) y
    static member (%)(BitVecExpr x, BitVecExpr y) = smod x y
    static member (%)(BitVecExpr x, y) = smod x (mkBitVec y x.SortSize)
    static member (%)(x, BitVecExpr y) = smod (mkBitVec x y.SortSize) y
    static member (<<<)(BitVecExpr x, BitVecExpr y) = shl x y
    static member (<<<)(BitVecExpr x, y) = shl x (mkBitVec y x.SortSize)
    static member (<<<)(x, BitVecExpr y) = shl (mkBitVec x y.SortSize) y
    static member (>>>)(BitVecExpr x, BitVecExpr y) = ashr x y
    static member (>>>)(BitVecExpr x, y) = ashr x (mkBitVec y x.SortSize)
    static member (>>>)(x, BitVecExpr y) = ashr (mkBitVec x y.SortSize) y
    static member (&&&)(BitVecExpr x, BitVecExpr y) = bvand x y
    static member (&&&)(BitVecExpr x, y) = bvand x (mkBitVec y x.SortSize)
    static member (&&&)(x, BitVecExpr y) = bvand (mkBitVec x y.SortSize) y
    static member (|||)(BitVecExpr x, BitVecExpr y) = bvor x y
    static member (|||)(BitVecExpr x, y) = bvor x (mkBitVec y x.SortSize)
    static member (|||)(x, BitVecExpr y) = bvor (mkBitVec x y.SortSize) y
    static member (~~~)(BitVecExpr x) = bvnot x 
    static member (>.)(BitVecExpr x, BitVecExpr y) = gt x y
    static member (>.)(BitVecExpr x, y) = gt x (mkBitVec y x.SortSize)
    static member (>.)(x, BitVecExpr y) = gt (mkBitVec x y.SortSize) y
    static member (=.)(BitVecExpr x, BitVecExpr y) = eq x y
    static member (=.)(BitVecExpr x, y) = eq x (mkBitVec y x.SortSize)
    static member (=.)(x, BitVecExpr y) = eq (mkBitVec x y.SortSize) y
    static member (>=.)(BitVecExpr x, BitVecExpr y) = ge x y
    static member (>=.)(BitVecExpr x, y) = ge x (mkBitVec y x.SortSize)
    static member (>=.)(x, BitVecExpr y) = ge (mkBitVec x y.SortSize) y
    static member (<.)(BitVecExpr x, BitVecExpr y) = lt x y
    static member (<.)(BitVecExpr x, y) = lt x (mkBitVec y x.SortSize)
    static member (<.)(x, BitVecExpr y) = lt (mkBitVec x y.SortSize) y
    static member (<>.)(BitVecExpr x, BitVecExpr y) = ueq x y
    static member (<>.)(BitVecExpr x, y) = ueq x (mkBitVec y x.SortSize)
    static member (<>.)(x, BitVecExpr y) = ueq (mkBitVec x y.SortSize) y
    static member (<=.)(BitVecExpr x, BitVecExpr y) = le x y
    static member (<=.)(BitVecExpr x, y) = le x (mkBitVec y x.SortSize)
    static member (<=.)(x, BitVecExpr y) = le (mkBitVec x y.SortSize) y
    static member Distinct xs = Array.map (fun (BitVecExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, BitVecExpr expr1, BitVecExpr expr2) = mkITE b expr1 expr2
    static member (<~)(BitVecExpr x, BitVecExpr y) = ult x y
    static member (<~)(BitVecExpr x, y) = ult x (mkBitVec y x.SortSize)
    static member (<~)(x, BitVecExpr y) = ult (mkBitVec x y.SortSize) y
    static member (<=~)(BitVecExpr x, BitVecExpr y) = ule x y
    static member (<=~)(BitVecExpr x, y) = ule x (mkBitVec y x.SortSize)
    static member (<=~)(x, BitVecExpr y) = ule (mkBitVec x y.SortSize) y
    static member (>~)(BitVecExpr x, BitVecExpr y) = ugt x y
    static member (>~)(BitVecExpr x, y) = ugt x (mkBitVec y x.SortSize)
    static member (>~)(x, BitVecExpr y) = ugt (mkBitVec x y.SortSize) y
    static member (>=~)(BitVecExpr x, BitVecExpr y) = uge x y
    static member (>=~)(BitVecExpr x, y) = uge x (mkBitVec y x.SortSize)
    static member (>=~)(x, BitVecExpr y) = uge (mkBitVec x y.SortSize) y
    static member (%~)(BitVecExpr x, BitVecExpr y) = umod x y
    static member (%~)(BitVecExpr x, y) = umod x (mkBitVec y x.SortSize)
    static member (%~)(x, BitVecExpr y) = umod (mkBitVec x y.SortSize) y
    static member (>>>~)(BitVecExpr x, BitVecExpr y) = lshr x y
    static member (>>>~)(BitVecExpr x, y) = lshr x (mkBitVec y x.SortSize)
    static member (>>>~)(x, BitVecExpr y) = lshr (mkBitVec x y.SortSize) y

let BitVec(name: string, size: uint32) =
    let context = getContext()
    context.MkBVConst(name, size) |> BitVecExpr

let BitVecVal(v: int, size: uint32) =
    mkBitVec v size :> BitVecExpr |> BitVecExpr

