#r "../Microsoft.Z3.dll"
#load "Context.fs"
#load "Bool.fs"
#load "Int.fs"
#load "Real.fs"

open Microsoft.Z3.FSharp
open Bool
open Int
open Real

let boolArithExample1() =
    let p = Bool "p"
    let q = Bool "q"
    let r = Bool "r"
    Z3.Solve(p =>. q, r <=>. (!. q), (!. p) ||. r)

let boolArithExample2() =
    let p = Bool("p")
    let q = Bool("q")
    let r = Bool("r")
    Z3.Solve(Implies(p, q), r <=>. Not(q), Or [|Not(p); r|])

let intArithExample1() =
    let x = Int "x"
    let y = Int "y"
    Z3.Solve(x >. 2I, y <. 10I, x + 2I*y =. 7I)

let realArithExample1() =
    let x = Real "x"
    let y = Real "y"
    Z3.Solve(x ** 2I + y ** 2I >. 3.0, x ** 3I + y <. 5.0)

let mixedArithExample1() =
    let p = Bool("p")
    let x = Real("x")
    Z3.Solve(Or [|x <. 5.0; x >. 10.0|], Or [|p; x ** 2I =. 2.0|], Not(p))

let mixedArithExample2() =
    let a = Int "a"
    let b = Int "b"
    let c = Int "c"
    let d = Real "d"
    let e = Real "e"
    Z3.Solve(a >. b + 2I, a =. 2I*c + 10I, c + b <=. 1000I, d >=. e)

let simplifyExample1() =
    let x = Real("x")
    let y = Real("y")
    // Using Z3 native option names
    printfn "%A" <| Z3.Simplify(x =. y + 2.0, ":arith-lhs" => true)
    
#time "on";;

let res00 = boolArithExample1();;
let res01 = boolArithExample2();;
let res02 = intArithExample1();;
let res03 = realArithExample1();;
let res04 = mixedArithExample1();;
let res05 = mixedArithExample2();;
let res06 = simplifyExample1();;
