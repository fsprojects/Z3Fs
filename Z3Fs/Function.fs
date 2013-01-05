module Microsoft.Z3.FSharp.Function

open System
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool

type Microsoft.Z3.FSharp.Bool.Z3 with
    static member inline CreateFunction< ^T when ^T: (static member FromExpr : Expr -> ^T) 
                                         and ^T: (static member Sort : Sort)>(name: string, [<ParamArray>] args: Theory []) =
        let exprs = args |> Array.map (fun arg -> arg.Expr)
        let sorts = exprs |> Array.map (fun expr -> expr.Sort)
        let range = (^T : (static member Sort : Sort) ())
        (^T : (static member FromExpr : Expr -> ^T) (getContext().MkFuncDecl(name, sorts, range).Apply(exprs)))