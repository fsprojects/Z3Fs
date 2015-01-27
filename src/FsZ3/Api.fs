[<AutoOpen>]
module Microsoft.Z3.Api

open Microsoft.Z3

module Context =
  open System.Collections.Generic

  /// Make a new context
  let create () =
    new Context(Dictionary ())

  /// Get a list of tactics for the context
  let tactics (ctx : Context) =
    ctx.TacticNames

/// Globals / global state
module Gs =
  let mutable private globalCtx = Context.create ()
  let context () = globalCtx

let inline (++) xs ys = Array.append xs ys

module BoolSort =
  let create (ctx : Context) = ctx.MkBoolSort()

module IntSort =
  let create (ctx : Context) = ctx.MkIntSort()

module RealSort =
  let create (ctx : Context) = ctx.MkRealSort()

module BitVecSort =
  let create (ctx : Context) i = ctx.MkBitVecSort i

module ArraySort =
  let create (ctx : Context) domain range = ctx.MkArraySort(domain, range)

[<AbstractClass>]
type Theory() =
  abstract member Expr: Expr

/// Implement IEnumerable interface to support for..in..do construct
type Microsoft.Z3.Statistics with
  member x.GetEnumerator() =
    (Seq.map (fun k -> k, x.[k]) x.Keys).GetEnumerator()

type Result =
  | Const of Expr
  | Func of FuncInterp
with
  override x.ToString() =
    match x with
    | Const expr -> sprintf "%O" expr
    | Func f -> sprintf "%O" f

/// Multiple indexers for evaluating formulas
type Microsoft.Z3.Model with
  member x.Item (index: Expr) =
    x.Eval(index, true)
  member x.Item (index: FuncDecl) =
    // Taking care of array declaration
    if index.DomainSize = 0u && index.Range.SortKind <> Z3_sort_kind.Z3_ARRAY_SORT
    then x.ConstInterp(index) |> Const
    else x.FuncInterp(index) |> Func
  member x.Evaluate(v: Theory, ?modelCompletion) =
    x.Evaluate(v.Expr, defaultArg modelCompletion false)

type SolveResult =
  | NoSolution
  | Unknown
  | Solution of (Symbol * FuncDecl * Result) list

module Solver =

  /// Make a new solver with an optional tactic
  let create (context : Context) =
    context.MkSolver()

  let create_tactic (context : Context) (tactic : string) =
    context.MkSolver tactic

  let private wrap_status (solver : Solver) = function
    | Status.SATISFIABLE ->
      let m       = solver.Model
      let results = m.Decls |> Array.map (fun d -> d.Name, d, m.[d])
      Solution (results |> List.ofArray)
    | Status.UNKNOWN ->
      Unknown
    | Status.UNSATISFIABLE ->
      NoSolution
    | x -> failwithf "unknown enum value %O" x

  let check (solver : Solver) =
    solver.Check () |> wrap_status solver

  let check_assumptions (solver : Solver) (assumptions : _ list) =
    solver.Check(Array.ofList assumptions) |> wrap_status solver