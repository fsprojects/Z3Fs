module Microsoft.Z3.Int

open System
open System.Numerics

open Microsoft.Z3
open Microsoft.Z3.Bool

type Int(expr: IntExpr) = 
  inherit Theory()
  override x.Expr = expr :> Expr
  override x.ToString() = sprintf "%O" expr
  static member FromExpr (e: Expr) = Int(e :?> IntExpr)

let IntExpr expr = Int(expr)
let (|IntExpr|) (i: Int) = i.Expr :?> IntExpr

[<AutoOpen>]
module internal IntUtils =
  let inline createInt (x: bigint) = Gs.context().MkInt(string x)
  let inline add x y = Gs.context().MkAdd(x, y) :?> IntExpr |> IntExpr
  let inline subtract x y = Gs.context().MkSub(x, y) :?> IntExpr |> IntExpr
  let inline multiply x y = Gs.context().MkMul(x, y) :?> IntExpr |> IntExpr
  let inline divide x y = Gs.context().MkDiv(x, y) :?> IntExpr |> IntExpr
  let inline exp x y =
          let rec loop i acc =
              if i = 0I then acc
              else loop (i-1I) (Gs.context().MkMul(acc, x))
          if y = 0I then (createInt 0I) :> IntExpr |> IntExpr
          elif y > 0I then loop (y-1I) x :?> IntExpr |> IntExpr
          else failwith "Coefficient should be a non-negative integer"
  let inline gt x y = Gs.context().MkGt(x, y) |> BoolExpr
  let inline eq x y = Gs.context().MkEq(x, y) |> BoolExpr
  let inline ge x y = Gs.context().MkGe(x, y) |> BoolExpr
  let inline lt x y = Gs.context().MkLt(x, y) |> BoolExpr
  let inline ueq x y = Gs.context().MkDistinct(x, y) |> BoolExpr
  let inline le x y = Gs.context().MkLe(x, y) |> BoolExpr
  let inline distinct xs = Gs.context().MkDistinct xs |> BoolExpr
  let inline createITE b expr1 expr2 = Gs.context().MkITE(b, expr1, expr2) :?> IntExpr |> IntExpr

type Int with
  static member (+)(IntExpr x, IntExpr y) = add x y
  static member (+)(IntExpr x, y) = add x (createInt y)
  static member (+)(x, IntExpr y) = add (createInt x) y
  static member (-)(IntExpr x, IntExpr y) = subtract x y
  static member (-)(IntExpr x, y) = subtract x (createInt y)
  static member (-)(x, IntExpr y) = subtract (createInt x) y
  static member (*)(IntExpr x, IntExpr y) = multiply x y
  static member (*)(IntExpr x, y) = multiply x (createInt y)
  static member (*)(x, IntExpr y) = multiply (createInt x) y
  static member (/)(IntExpr x, IntExpr y) = divide x y
  static member (/)(IntExpr x, y) = divide x (createInt y)
  static member (/)(x, IntExpr y) = divide (createInt x) y  
  static member Pow(IntExpr x, y) = exp x y // use this name instead of ( ** )
  static member (>.)(IntExpr x, IntExpr y) = gt x y
  static member (>.)(IntExpr x, y) = gt x (createInt y)
  static member (>.)(x, IntExpr y) = gt (createInt x) y
  static member (=.)(IntExpr x, IntExpr y) = eq x y
  static member (=.)(IntExpr x, y) = eq x (createInt y)
  static member (=.)(x, IntExpr y) = eq (createInt x) y 
  static member (>=.)(IntExpr x, IntExpr y) = ge x y
  static member (>=.)(IntExpr x, y) = ge x (createInt y)
  static member (>=.)(x, IntExpr y) = ge (createInt x) y 
  static member (<.)(IntExpr x, IntExpr y) = lt x y
  static member (<.)(IntExpr x, y) = lt x (createInt y)
  static member (<.)(x, IntExpr y) = lt (createInt x) y
  static member (<>.)(IntExpr x, IntExpr y) = ueq x y
  static member (<>.)(IntExpr x, y) = ueq x (createInt y)
  static member (<>.)(x, IntExpr y) = ueq (createInt x) y 
  static member (<=.)(IntExpr x, IntExpr y) = le x y
  static member (<=.)(IntExpr x, y) = le x (createInt y)
  static member (<=.)(x, IntExpr y) = le (createInt x) y
  static member Distinct xs = Array.map (fun (IntExpr expr) -> expr :> Expr) xs |> createDistinct
  static member If(BoolExpr b, IntExpr expr1, IntExpr expr2) = createITE b expr1 expr2

/// Return an int const with supplied name
let Int(s: string) = 
  let context = Gs.context()
  context.MkIntConst s |> IntExpr

let IntVal(i: bigint) = 
  let context = Gs.context()
  context.MkInt(i.ToString()) :> IntExpr |> IntExpr