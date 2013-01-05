module Microsoft.Z3.FSharp.Bool

open System
open System.Numerics

open Microsoft.Z3
open Microsoft.Z3.FSharp.Common

type Bool(e: BoolExpr) = 
    inherit Theory()
    override x.Expr = e :> Expr
    override x.ToString() = sprintf "%O" e
    static member FromExpr (e: Expr) = Bool(e :?> BoolExpr)

let BoolExpr expr = Bool(expr)
let (|BoolExpr|) (b: Bool) = b.Expr :?> BoolExpr

type Microsoft.Z3.Solver with
    member x.Add([<ParamArray>] xs: _ []) =
        for (BoolExpr expr) in xs do 
          x.Assert expr

[<AutoOpen>]
module internal BoolUtils =
    let inline mkBool b = getContext().MkBool(b)
    let inline mkAnd x y = getContext().MkAnd(x, y) |> BoolExpr
    let inline mkOr x y = getContext().MkOr(x, y) |> BoolExpr
    let inline mkNot x = getContext().MkNot(x) |> BoolExpr
    let inline mkImplies x y = getContext().MkImplies(x, y) |> BoolExpr
    let inline mkEquiv x y = getContext().MkEq(x, y) |> BoolExpr
    let inline mkTrue() = getContext().MkTrue() |> BoolExpr
    let inline mkFalse() = getContext().MkFalse() |> BoolExpr
    let inline mkDistinct (xs: Expr []) = getContext().MkDistinct xs |> BoolExpr
    let inline mkITE b expr1 expr2 = getContext().MkITE(b, expr1, expr2) :?> BoolExpr |> BoolExpr
    
type Bool with    
    static member (&&.)(BoolExpr p, BoolExpr q) = mkAnd p q    
    static member (&&.)(BoolExpr p, q) = mkAnd p (mkBool q)
    static member (&&.)(p, BoolExpr q) = mkAnd (mkBool p) q
    static member (||.)(BoolExpr p, BoolExpr q) = mkOr p q
    static member (||.)(BoolExpr p, q) = mkOr p (mkBool q)
    static member (||.)(p, BoolExpr q) = mkOr (mkBool p) q
    static member (!.) (BoolExpr p) = mkNot p
    static member (=>.)(BoolExpr p, BoolExpr q) = mkImplies p q
    static member (=>.)(BoolExpr p, q) = mkImplies p (mkBool q)
    static member (=>.)(p, BoolExpr q) = mkImplies (mkBool p) q    
    static member (=.)(BoolExpr p, BoolExpr q) = mkEquiv p q
    static member (=.)(BoolExpr p, q) = mkEquiv p (mkBool q)
    static member (=.)(p, BoolExpr q) = mkEquiv (mkBool p) q
    static member Distinct xs = Array.map (fun (BoolExpr expr) -> expr :> Expr) xs |> mkDistinct
    static member If(BoolExpr b, BoolExpr expr1, BoolExpr expr2) = mkITE b expr1 expr2

/// Return a bool const with supplied name
let Bool(s: string) = 
    let context = getContext()
    context.MkBoolConst s |> BoolExpr

let True = mkTrue()
let False = mkFalse()

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
    let p = getContext().MkParams()
    for (k, v) in options do
        match v with
        | Bool b -> p.Add(k, b)
        | UInt i -> p.Add(k, i)
        | Double f -> p.Add(k, f)
    f.Simplify(p)

let internal setOption (options: (string * _) []) =
    let c = getContext()
    let p = c.MkParams()
    for (k, v) in options do
        match v with
        | Bool b -> c.UpdateParamValue(k, sprintf "%O" b)
        | UInt i -> c.UpdateParamValue(k, sprintf "%O" i)
        | Double f -> c.UpdateParamValue(k, sprintf "%O" f)

type Z3 =
    static member Solve ([<ParamArray>] xs: _ []) =
        let solver = getContext().MkSolver()
        for (BoolExpr expr) in xs do
            solver.Assert expr
        let result = solver.Check()
        if result = Status.SATISFIABLE then
            let m = solver.Model
            printfn "["
            m.Decls
            |> Seq.map (fun d -> sprintf " %O = %O" d.Name m.[d])
            |> fun s -> String.Join(",\n", s)
            |> printfn "%s"
            printfn "]"
        else printfn "No solution"
        result

    static member inline Simplify(f: ^T when ^T :> Theory and ^T: (static member FromExpr : Expr -> ^T), 
                                    [<ParamArray>] options: (string * _) []) = 
        (^T : (static member FromExpr : Expr -> ^T) (simplify f.Expr options))

    static member SetOption([<ParamArray>] options: (string * _) []) = setOption(options)

        
