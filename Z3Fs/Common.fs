module Microsoft.Z3.FSharp.Common

open System.Collections.Generic
open Microsoft.Z3

let mutable private context = new Context(Dictionary())
let getContext() = context
let setContext c = context <- c
    
let Solver() = getContext().MkSolver()