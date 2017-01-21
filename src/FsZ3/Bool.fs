module Microsoft.Z3.Bool

open System
open System.Numerics

open Microsoft.Z3

type Bool(e: BoolExpr) = 
  inherit Theory()
  override x.Expr = e :> Expr
  override x.ToString() = sprintf "%O" e
  static member FromExpr (e: Expr) = Bool(e :?> BoolExpr)

let BoolExpr expr = Bool(expr)
let (|BoolExpr|) (b: Bool) = b.Expr :?> BoolExpr

module Solver =

  let ``assert`` (x : Solver) (BoolExpr expr) =
    x.Assert expr

  let assert_range (x : Solver) (xs : _ []) =
    for BoolExpr e in xs do
      x.Assert e

[<AutoOpen>]
module internal BoolUtils =
  let inline createBool b = Gs.context().MkBool(b)
  let inline createAnd x y = Gs.context().MkAnd(x, y) |> BoolExpr
  let inline createOr x y = Gs.context().MkOr(x, y) |> BoolExpr
  let inline createNot x = Gs.context().MkNot(x) |> BoolExpr
  let inline createImplies x y = Gs.context().MkImplies(x, y) |> BoolExpr
  let inline createEquiv x y = Gs.context().MkEq(x, y) |> BoolExpr
  let inline createTrue() = Gs.context().MkTrue() |> BoolExpr
  let inline createFalse() = Gs.context().MkFalse() |> BoolExpr
  let inline createDistinct (xs: Expr []) = Gs.context().MkDistinct xs |> BoolExpr
  let inline createITE b expr1 expr2 = Gs.context().MkITE(b, expr1, expr2) :?> BoolExpr |> BoolExpr
    
type Bool with    
  static member (&&.)(BoolExpr p, BoolExpr q) = createAnd p q
  static member (&&.)(BoolExpr p, q) = createAnd p (createBool q)
  static member (&&.)(p, BoolExpr q) = createAnd (createBool p) q
  static member (||.)(BoolExpr p, BoolExpr q) = createOr p q
  static member (||.)(BoolExpr p, q) = createOr p (createBool q)
  static member (||.)(p, BoolExpr q) = createOr (createBool p) q
  static member (!.) (BoolExpr p) = createNot p
  static member (=>.)(BoolExpr p, BoolExpr q) = createImplies p q
  static member (=>.)(BoolExpr p, q) = createImplies p (createBool q)
  static member (=>.)(p, BoolExpr q) = createImplies (createBool p) q
  static member (=.)(BoolExpr p, BoolExpr q) = createEquiv p q
  static member (=.)(BoolExpr p, q) = createEquiv p (createBool q)
  static member (=.)(p, BoolExpr q) = createEquiv (createBool p) q
  static member Distinct xs = Array.map (fun (BoolExpr expr) -> expr :> Expr) xs |> createDistinct
  static member If(BoolExpr b, BoolExpr expr1, BoolExpr expr2) = createITE b expr1 expr2

/// Return a bool const with supplied name
let Bool(s: string) = 
  let context = Gs.context()
  context.MkBoolConst s |> BoolExpr

let True = createTrue()
let False = createFalse()

let And (args: Bool []) = Array.reduce (&&.) args
let Or (args: Bool []) = Array.reduce (||.) args
let Implies (arg1: Bool, arg2: Bool) = arg1 =>. arg2
let Not (arg: Bool) = !. arg

let inline Distinct (xs: ^T []) =
  (^T : (static member Distinct : ^T [] -> Bool) (xs))

let inline If (b: Bool, expr1: ^T, expr2: ^T) = 
  (^T : (static member If : Bool * ^T * ^T -> Bool) (b, expr1, expr2))

type Val =
  | Bool of bool
  | UInt of uint32
  | Double of float

type Overloads = Overloads with
  static member ($)(Overloads, b) = fun (s: string) -> s, Bool b
  static member ($)(Overloads, i) = fun (s: string) -> s, UInt i
  static member ($)(Overloads, f) = fun (s: string) -> s, Double f

let inline (=>) k v = (Overloads $ v) k

let simplify (f: Expr) (options: (string * _) []) =
  let p = Gs.context().MkParams()
  for (k, v) in options do
    match v with
    | Bool b -> p.Add(k, b)
    | UInt i -> p.Add(k, i)
    | Double f -> p.Add(k, f)
  f.Simplify(p)

let internal set_option (options: (string * _) []) =
  let c = Gs.context()
  let p = c.MkParams()
  for (k, v) in options do
    match v with
    | Bool b -> c.UpdateParamValue(k, sprintf "%O" b)
    | UInt i -> c.UpdateParamValue(k, sprintf "%O" i)
    | Double f -> c.UpdateParamValue(k, sprintf "%O" f)

type Z3 =
  static member Solve ([<ParamArray>] xs: _ []) =
    let solver = Gs.context() |> Solver.create
    xs |> Solver.assert_range solver
    let sol = Solver.check solver
    match sol with
    | NoSolution
    | Unknown ->
      printfn "no solution"
    | Solution solutions ->
      let m = solver.Model
      printfn "["
      solutions
      |> Seq.map (fun (name, decl, value) -> sprintf " %O = %O" name value)
      |> fun s -> String.Join(",\n", s)
      |> printfn "%s"
      printfn "]"
    sol

  static member SolveResults ([<ParamArray>] xs: _ []) =
    let solver = Gs.context() |> Solver.create
    xs |> Solver.assert_range solver
    Solver.check solver

  static member inline Simplify(f: ^T when ^T :> Theory and ^T: (static member FromExpr : Expr -> ^T),
                                  [<ParamArray>] options: (string * _) []) = 
    (^T : (static member FromExpr : Expr -> ^T) (simplify f.Expr options))

  static member SetOption([<ParamArray>] options: (string * _) []) =
    set_option options