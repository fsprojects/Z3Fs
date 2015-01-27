module Microsoft.Z3.Real

open System
open Microsoft.Z3
open Microsoft.Z3.Bool

type Real(expr: RealExpr) = 
  inherit Theory()
  override x.Expr = expr :> Expr
  override x.ToString() = sprintf "%O" expr
  static member FromExpr (e: Expr) = Real(e :?> RealExpr)

let RealExpr expr = Real(expr)
let (|RealExpr|) (r: Real) = r.Expr :?> RealExpr

[<AutoOpen>]
module internal RealUtils =
  let inline fromFloat (x: float) = Gs.context().MkReal(string x)
  let inline fromDecimal (x: decimal) = Gs.context().MkReal(string x)
  let inline add x y = Gs.context().MkAdd(x, y) :?> RealExpr |> RealExpr
  let inline subtract x y = Gs.context().MkSub(x, y) :?> RealExpr |> RealExpr
  let inline multiply x y = Gs.context().MkMul(x, y) :?> RealExpr |> RealExpr
  let inline divide x y = Gs.context().MkDiv(x, y) :?> RealExpr |> RealExpr
  let inline exp x y =
          let rec loop i acc =
              if i = 0I then acc
              else loop (i-1I) (Gs.context().MkMul(acc, x))
          if y = 0I then (fromFloat 0.) :> RealExpr |> RealExpr
          elif y > 0I then loop (y-1I) x :?> RealExpr |> RealExpr
          else failwith "Coefficient should be a non-negative integer"
  let inline gt x y = Gs.context().MkGt(x, y) |> BoolExpr
  let inline eq x y = Gs.context().MkEq(x, y) |> BoolExpr
  let inline ge x y = Gs.context().MkGe(x, y) |> BoolExpr
  let inline lt x y = Gs.context().MkLt(x, y) |> BoolExpr
  let inline ueq x y = Gs.context().MkDistinct(x, y) |> BoolExpr
  let inline le x y = Gs.context().MkLe(x, y) |> BoolExpr
  let inline distinct (xs: Expr []) = Gs.context().MkDistinct xs |> BoolExpr
  let inline createITE b expr1 expr2 = Gs.context().MkITE(b, expr1, expr2) :?> RealExpr |> RealExpr

type Real with
  static member (+)(RealExpr x, RealExpr y) = add x y
  static member (+)(RealExpr x, y) = add x (fromFloat y)
  static member (+)(x, RealExpr y) = add (fromFloat x) y
  static member (+)(RealExpr x, y) = add x (fromDecimal y)
  static member (+)(x, RealExpr y) = add (fromDecimal x) y
  static member (-)(RealExpr x, RealExpr y) = subtract x y
  static member (-)(RealExpr x, y) = subtract x (fromFloat y)
  static member (-)(x, RealExpr y) = subtract (fromFloat x) y
  static member (-)(RealExpr x, y) = subtract x (fromDecimal y)
  static member (-)(x, RealExpr y) = subtract (fromDecimal x) y
  static member (*)(RealExpr x, RealExpr y) = multiply x y
  static member (*)(RealExpr x, y) = multiply x (fromFloat y)
  static member (*)(x, RealExpr y) = multiply (fromFloat x) y
  static member (*)(RealExpr x, y) = multiply x (fromDecimal y)
  static member (*)(x, RealExpr y) = multiply (fromDecimal x) y
  static member (/)(RealExpr x, RealExpr y) = divide x y
  static member (/)(RealExpr x, y) = divide x (fromFloat y)
  static member (/)(x, RealExpr y) = divide (fromFloat x) y
  static member (/)(RealExpr x, y) = divide x (fromDecimal y)
  static member (/)(x, RealExpr y) = divide (fromDecimal x) y
  static member Pow(RealExpr x, y) = exp x y
  static member (>.)(RealExpr x, RealExpr y) = gt x y
  static member (>.)(RealExpr x, y) = gt x (fromFloat y)
  static member (>.)(x, RealExpr y) = gt (fromFloat x) y
  static member (>.)(RealExpr x, y) = gt x (fromDecimal y)
  static member (>.)(x, RealExpr y) = gt (fromDecimal x) y
  static member (=.)(RealExpr x, RealExpr y) = eq x y
  static member (=.)(RealExpr x, y) = eq x (fromFloat y)
  static member (=.)(x, RealExpr y) = eq (fromFloat x) y
  static member (=.)(RealExpr x, y) = eq x (fromDecimal y)
  static member (=.)(x, RealExpr y) = eq (fromDecimal x) y
  static member (>=.)(RealExpr x, RealExpr y) = ge x y
  static member (>=.)(RealExpr x, y) = ge x (fromFloat y)
  static member (>=.)(x, RealExpr y) = ge (fromFloat x) y
  static member (>=.)(RealExpr x, y) = ge x (fromDecimal y)
  static member (>=.)(x, RealExpr y) = ge (fromDecimal x) y
  static member (<.)(RealExpr x, RealExpr y) = lt x y
  static member (<.)(RealExpr x, y) = lt x (fromFloat y)
  static member (<.)(x, RealExpr y) = lt (fromFloat x) y
  static member (<.)(RealExpr x, y) = lt x (fromDecimal y)
  static member (<.)(x, RealExpr y) = lt (fromDecimal x) y
  static member (<>.)(RealExpr x, RealExpr y) = ueq x y
  static member (<>.)(RealExpr x, y) = ueq x (fromFloat y)
  static member (<>.)(x, RealExpr y) = ueq (fromFloat x) y
  static member (<>.)(RealExpr x, y) = ueq x (fromDecimal y)
  static member (<>.)(x, RealExpr y) = ueq (fromDecimal x) y
  static member (<=.)(RealExpr x, RealExpr y) = le x y
  static member (<=.)(RealExpr x, y) = le x (fromFloat y)
  static member (<=.)(x, RealExpr y) = le (fromFloat x) y
  static member (<=.)(RealExpr x, y) = le x (fromDecimal y)
  static member (<=.)(x, RealExpr y) = le (fromDecimal x) y
  static member Distinct xs = Array.map (fun (RealExpr expr) -> expr :> Expr) xs |> distinct
  static member If(BoolExpr b, RealExpr expr1, RealExpr expr2) = createITE b expr1 expr2

/// Return a real const with supplied name
let Real(s: string) =
  let context = Gs.context()
  context.MkRealConst s |> RealExpr

let RealVal(f: float) =
  let context = Gs.context()
  context.MkReal(f.ToString()) :> RealExpr |> RealExpr