module Microsoft.Z3.BitVec

open System
open Microsoft.Z3
open Microsoft.Z3.Bool

type BitVec(expr: BitVecExpr) = 
  inherit Theory()
  override x.Expr = expr :> Expr
  override x.ToString() = sprintf "%O" expr
  static member FromExpr (e: Expr) = BitVec(e :?> BitVecExpr)

let BitVecExpr expr = BitVec(expr)
let (|BitVecExpr|) (bv: BitVec) = bv.Expr :?> BitVecExpr

[<AutoOpen>]
module internal BitVecUtils =
    let inline createBitVec (v: int) (size: uint32) = Gs.context().MkBV(v, size)
    let inline add x y = Gs.context().MkBVAdd(x, y) |> BitVecExpr
    let inline subtract x y = Gs.context().MkBVSub(x, y) |> BitVecExpr
    let inline multiply x y = Gs.context().MkBVMul(x, y) |> BitVecExpr
    let inline sdiv x y = Gs.context().MkBVSDiv(x, y) |> BitVecExpr
    let inline smod x y = Gs.context().MkBVSMod(x, y) |> BitVecExpr
    let inline shl x y = Gs.context().MkBVSHL(x, y) |> BitVecExpr
    let inline ashr x y = Gs.context().MkBVASHR(x, y) |> BitVecExpr
    let inline bvand x y = Gs.context().MkBVAND(x, y) |> BitVecExpr
    let inline bvor x y = Gs.context().MkBVOR(x, y) |> BitVecExpr
    let inline bvnot x = Gs.context().MkBVNeg(x) |> BitVecExpr
    let inline gt x y = Gs.context().MkBVSGT(x, y) |> BoolExpr
    let inline eq x y = Gs.context().MkEq(x, y) |> BoolExpr
    let inline ge x y = Gs.context().MkBVSGE(x, y) |> BoolExpr
    let inline lt x y = Gs.context().MkBVSLT(x, y) |> BoolExpr
    let inline ueq x y = Gs.context().MkDistinct(x, y) |> BoolExpr
    let inline le x y = Gs.context().MkBVSLE(x, y) |> BoolExpr
    let inline distinct xs = Gs.context().MkDistinct xs |> BoolExpr
    let inline createITE b expr1 expr2 = Gs.context().MkITE(b, expr1, expr2) :?> BitVecExpr |> BitVecExpr
    let inline ult x  y = Gs.context().MkBVULT(x, y) |> BoolExpr
    let inline ule x y = Gs.context().MkBVULT(x, y) |> BoolExpr
    let inline ugt x y = Gs.context().MkBVULT(x, y) |> BoolExpr
    let inline uge x y = Gs.context().MkBVULT(x, y) |> BoolExpr
    let inline udiv x y = Gs.context().MkBVUDiv(x, y) |> BitVecExpr
    let inline umod x y = Gs.context().MkBVURem(x, y) |> BitVecExpr
    let inline lshr x y = Gs.context().MkBVLSHR(x, y) |> BitVecExpr

type BitVec with
    static member (+)(BitVecExpr x, BitVecExpr y) = add x y
    static member (+)(BitVecExpr x, y) = add x (createBitVec y x.SortSize)
    static member (+)(x, BitVecExpr y) = add (createBitVec x y.SortSize) y
    static member (-)(BitVecExpr x, BitVecExpr y) = subtract x y
    static member (-)(BitVecExpr x, y) = subtract x (createBitVec y x.SortSize)
    static member (-)(x, BitVecExpr y) = subtract (createBitVec x y.SortSize) y
    static member (*)(BitVecExpr x, BitVecExpr y) = multiply x y
    static member (*)(BitVecExpr x, y) = multiply x (createBitVec y x.SortSize)
    static member (*)(x, BitVecExpr y) = multiply (createBitVec x y.SortSize) y
    static member (/)(BitVecExpr x, BitVecExpr y) = sdiv x y
    static member (/)(BitVecExpr x, y) = sdiv x (createBitVec y x.SortSize)
    static member (/)(x, BitVecExpr y) = sdiv (createBitVec x y.SortSize) y
    static member (%)(BitVecExpr x, BitVecExpr y) = smod x y
    static member (%)(BitVecExpr x, y) = smod x (createBitVec y x.SortSize)
    static member (%)(x, BitVecExpr y) = smod (createBitVec x y.SortSize) y
    static member (<<<)(BitVecExpr x, BitVecExpr y) = shl x y
    static member (<<<)(BitVecExpr x, y) = shl x (createBitVec y x.SortSize)
    static member (<<<)(x, BitVecExpr y) = shl (createBitVec x y.SortSize) y
    static member (>>>)(BitVecExpr x, BitVecExpr y) = ashr x y
    static member (>>>)(BitVecExpr x, y) = ashr x (createBitVec y x.SortSize)
    static member (>>>)(x, BitVecExpr y) = ashr (createBitVec x y.SortSize) y
    static member (&&&)(BitVecExpr x, BitVecExpr y) = bvand x y
    static member (&&&)(BitVecExpr x, y) = bvand x (createBitVec y x.SortSize)
    static member (&&&)(x, BitVecExpr y) = bvand (createBitVec x y.SortSize) y
    static member (|||)(BitVecExpr x, BitVecExpr y) = bvor x y
    static member (|||)(BitVecExpr x, y) = bvor x (createBitVec y x.SortSize)
    static member (|||)(x, BitVecExpr y) = bvor (createBitVec x y.SortSize) y
    static member (~~~)(BitVecExpr x) = bvnot x 
    static member (>.)(BitVecExpr x, BitVecExpr y) = gt x y
    static member (>.)(BitVecExpr x, y) = gt x (createBitVec y x.SortSize)
    static member (>.)(x, BitVecExpr y) = gt (createBitVec x y.SortSize) y
    static member (=.)(BitVecExpr x, BitVecExpr y) = eq x y
    static member (=.)(BitVecExpr x, y) = eq x (createBitVec y x.SortSize)
    static member (=.)(x, BitVecExpr y) = eq (createBitVec x y.SortSize) y
    static member (>=.)(BitVecExpr x, BitVecExpr y) = ge x y
    static member (>=.)(BitVecExpr x, y) = ge x (createBitVec y x.SortSize)
    static member (>=.)(x, BitVecExpr y) = ge (createBitVec x y.SortSize) y
    static member (<.)(BitVecExpr x, BitVecExpr y) = lt x y
    static member (<.)(BitVecExpr x, y) = lt x (createBitVec y x.SortSize)
    static member (<.)(x, BitVecExpr y) = lt (createBitVec x y.SortSize) y
    static member (<>.)(BitVecExpr x, BitVecExpr y) = ueq x y
    static member (<>.)(BitVecExpr x, y) = ueq x (createBitVec y x.SortSize)
    static member (<>.)(x, BitVecExpr y) = ueq (createBitVec x y.SortSize) y
    static member (<=.)(BitVecExpr x, BitVecExpr y) = le x y
    static member (<=.)(BitVecExpr x, y) = le x (createBitVec y x.SortSize)
    static member (<=.)(x, BitVecExpr y) = le (createBitVec x y.SortSize) y
    static member Distinct xs = Array.map (fun (BitVecExpr expr) -> expr :> Expr) xs |> createDistinct
    static member If(BoolExpr b, BitVecExpr expr1, BitVecExpr expr2) = createITE b expr1 expr2
    static member (<~)(BitVecExpr x, BitVecExpr y) = ult x y
    static member (<~)(BitVecExpr x, y) = ult x (createBitVec y x.SortSize)
    static member (<~)(x, BitVecExpr y) = ult (createBitVec x y.SortSize) y
    static member (<=~)(BitVecExpr x, BitVecExpr y) = ule x y
    static member (<=~)(BitVecExpr x, y) = ule x (createBitVec y x.SortSize)
    static member (<=~)(x, BitVecExpr y) = ule (createBitVec x y.SortSize) y
    static member (>~)(BitVecExpr x, BitVecExpr y) = ugt x y
    static member (>~)(BitVecExpr x, y) = ugt x (createBitVec y x.SortSize)
    static member (>~)(x, BitVecExpr y) = ugt (createBitVec x y.SortSize) y
    static member (>=~)(BitVecExpr x, BitVecExpr y) = uge x y
    static member (>=~)(BitVecExpr x, y) = uge x (createBitVec y x.SortSize)
    static member (>=~)(x, BitVecExpr y) = uge (createBitVec x y.SortSize) y
    static member (%~)(BitVecExpr x, BitVecExpr y) = umod x y
    static member (%~)(BitVecExpr x, y) = umod x (createBitVec y x.SortSize)
    static member (%~)(x, BitVecExpr y) = umod (createBitVec x y.SortSize) y
    static member (>>>~)(BitVecExpr x, BitVecExpr y) = lshr x y
    static member (>>>~)(BitVecExpr x, y) = lshr x (createBitVec y x.SortSize)
    static member (>>>~)(x, BitVecExpr y) = lshr (createBitVec x y.SortSize) y

let BitVec(name: string, size: uint32) =
  let context = Gs.context()
  context.MkBVConst(name, size) |> BitVecExpr

let BitVecVal(v: int, size: uint32) =
  createBitVec v size :> BitVecExpr |> BitVecExpr