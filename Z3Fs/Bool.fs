module Microsoft.Z3.FSharp.Bool

open System
open System.Numerics
open Microsoft.Z3

module Z3 = Microsoft.Z3.FSharp.Context

type BoolArith = BoolExpr of BoolExpr

[<AutoOpen>]
module internal BoolUtils =
    let inline mkBool b = Z3.getContext().MkBool(b)
    let inline mkAnd (x: BoolExpr) (y: BoolExpr) = Z3.getContext().MkAnd(x, y) |> BoolExpr
    let inline mkOr (x: BoolExpr) (y: BoolExpr) = Z3.getContext().MkOr(x, y) |> BoolExpr
    let inline mkNot (x: BoolExpr) = Z3.getContext().MkNot(x) |> BoolExpr
    let inline mkImplies (x: BoolExpr) (y: BoolExpr) = Z3.getContext().MkImplies(x, y) |> BoolExpr
    let inline mkEquiv (x: BoolExpr) (y: BoolExpr) = Z3.getContext().MkEq(x, y) |> BoolExpr

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

let internal mkBoolVar =
    let context = Z3.getContext()
    fun (s: string) -> context.MkBoolConst s 

/// Return an int const with supplied name
let Bool = mkBoolVar >> BoolExpr

let True = Z3.getContext().MkTrue() |> BoolExpr
let False = Z3.getContext().MkFalse() |> BoolExpr

let And (args: BoolArith []) = Array.reduce (&&.) args
let Or (args: BoolArith []) = Array.reduce (||.) args
let Implies (arg1: BoolArith, arg2: BoolArith) = arg1 =>. arg2
let Not (arg: BoolArith) = !. arg

type Z3 =
    static member Solve ([<ParamArray>] xs: _ []) =
        let solver = Z3.getContext().MkSolver()
        for (BoolExpr expr) in xs do
            solver.Assert expr
        solver.Check()