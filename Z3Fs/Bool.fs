module Microsoft.Z3.FSharp.Bool

open System
open System.Numerics

open Microsoft.Z3
open Microsoft.Z3.FSharp.Common

type BoolArith = BoolExpr of BoolExpr
with member x.Expr = match x with BoolExpr expr -> expr
     override x.ToString() = match x with BoolExpr expr -> sprintf "%O" expr

[<AutoOpen>]
module internal BoolUtils =
    let inline mkBool b = getContext().MkBool(b)
    let inline mkAnd (x: BoolExpr) (y: BoolExpr) = getContext().MkAnd(x, y) |> BoolExpr
    let inline mkOr (x: BoolExpr) (y: BoolExpr) = getContext().MkOr(x, y) |> BoolExpr
    let inline mkNot (x: BoolExpr) = getContext().MkNot(x) |> BoolExpr
    let inline mkImplies (x: BoolExpr) (y: BoolExpr) = getContext().MkImplies(x, y) |> BoolExpr
    let inline mkEquiv (x: BoolExpr) (y: BoolExpr) = getContext().MkEq(x, y) |> BoolExpr
    let inline mkTrue() = getContext().MkTrue() |> BoolExpr
    let inline mkFalse() = getContext().MkFalse() |> BoolExpr
    let inline mkDistinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    
type BoolArith with    
    static member (&&.)(BoolExpr p, BoolExpr q) = mkAnd p q    
    static member (&&.)(BoolExpr p, q: bool) = mkAnd p (mkBool q)
    static member (&&.)(p: bool, BoolExpr q) = mkAnd (mkBool p) q
    static member (&&.)(p: bool, q: bool) = mkAnd (mkBool p) (mkBool q)
    static member (||.)(BoolExpr p, BoolExpr q) = mkOr p q
    static member (||.)(BoolExpr p, q: bool) = mkOr p (mkBool q)
    static member (||.)(p: bool, BoolExpr q) = mkOr (mkBool p) q
    static member (||.)(p: bool, q: bool) = mkOr (mkBool p) (mkBool q)
    static member (!.) (BoolExpr p) = mkNot p
    static member (!.) (p: bool) = mkNot (mkBool p)
    static member (=>.)(BoolExpr p, BoolExpr q) = mkImplies p q
    static member (=>.)(BoolExpr p, q: bool) = mkImplies p (mkBool q)
    static member (=>.)(p: bool, BoolExpr q) = mkImplies (mkBool p) q
    static member (=>.)(p: bool, q: bool) = mkImplies (mkBool p) (mkBool q)    
    static member (<=>.)(BoolExpr p, BoolExpr q) = mkEquiv p q
    static member (<=>.)(BoolExpr p, q: bool) = mkEquiv p (mkBool q)
    static member (<=>.)(p: bool, BoolExpr q) = mkEquiv (mkBool p) q
    static member (<=>.)(p: bool, q: bool) = mkEquiv (mkBool p) (mkBool q)
    static member (=.)(BoolExpr p, BoolExpr q) = mkEquiv p q
    static member (=.)(BoolExpr p, q: bool) = mkEquiv p (mkBool q)
    static member (=.)(p: bool, BoolExpr q) = mkEquiv (mkBool p) q
    static member (=.)(p: bool, q: bool) = mkEquiv (mkBool p) (mkBool q)
    static member Distinct xs = Array.map (fun (BoolExpr expr) -> expr :> Expr) xs |> mkDistinct

let internal mkBoolVar =
    let context = getContext()
    fun (s: string) -> context.MkBoolConst s 

/// Return an int const with supplied name
let Bool = mkBoolVar >> BoolExpr

let True = mkTrue()
let False = mkFalse()

let And (args: BoolArith []) = Array.reduce (&&.) args
let Or (args: BoolArith []) = Array.reduce (||.) args
let Implies (arg1: BoolArith, arg2: BoolArith) = arg1 =>. arg2
let Not (arg: BoolArith) = !. arg

let inline Distinct (xs: ^T []) = (^T : (static member Distinct : ^T [] -> BoolArith) (xs)) 

type Val =
    | Bool of bool
    | UInt of uint32
    | Double of float

type Overloads = Overloads with
    static member ($)(Overloads, b: bool) = fun (s: string) -> s, Bool b
    static member ($)(Overloads, i: uint32) = fun (s: string) -> s, UInt i
    static member ($)(Overloads, f: float) = fun (s: string) -> s, Double f

let inline (=>) k v = (Overloads $ v) k

let inline simplify (f: #Expr, options: (string * _) []) =
    let p = getContext().MkParams()
    for (k, v) in options do
        match v with
        | Bool b -> p.Add(k, b)
        | UInt i -> p.Add(k, i)
        | Double f -> p.Add(k, f)
    f.Simplify(p)

type Z3 =
    static member Solve ([<ParamArray>] xs: _ []) =
        let solver = getContext().MkSolver()
        for (BoolExpr expr) in xs do
            solver.Assert expr
        solver.Check()

    static member Simplify(BoolExpr f, [<ParamArray>] options: (string * _) []) = 
        simplify(f, options) :?> BoolExpr |> BoolExpr

type Microsoft.Z3.Solver with
    member x.Add([<ParamArray>] xs: _ []) =
        for (BoolExpr expr) in xs do 
          x.Assert expr

/// Implement IEnumerable interface to support for..in..do construct 
type Microsoft.Z3.Statistics with
    member x.GetEnumerator() =
        (Seq.map (fun k -> k, x.[k]) x.Keys).GetEnumerator()

/// Multiple indexers for evaluating formulas
type Microsoft.Z3.Model with
    member x.Item 
        with get (index: Expr) = x.Eval(index, true)    
    member x.Item 
        with get (index: FuncDecl) = x.ConstInterp(index)   
        
