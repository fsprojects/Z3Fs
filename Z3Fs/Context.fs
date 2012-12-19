namespace Microsoft.Z3.FSharp

open System.Collections.Generic
open Microsoft.Z3

[<RequireQualifiedAccess>]
module Context =
    let mutable internal context = new Context(Dictionary())
    let getContext() = context
    let setContext c = context <- c
    

