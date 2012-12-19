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
    Z3.Solve(x >. 2I, y <. 10I, x + 2I * y =. 7I)

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
    Z3.Solve(a >. b + 2I, a =. 2I * c + 10I, c + b <=. 1000I, d >=. e)

let simplifyExample1() =
    let x = Real("x")
    let y = Real("y")
    // Using Z3 native option names
    printfn "%A" <| Z3.Simplify(x =. y + 2.0, ":arith-lhs" => true)

let solverExample1() =
    let x = Int("x")
    let y = Int("y")

    let s = Solver()
    printfn "%O" s

    s.Add(x >. 10I, y =. x + 2I)
    printfn "%O" s
    printfn "Solving constraints in the solver s ..."
    printfn "%O" <| s.Check()

    printfn "Create a new scope..."
    s.Push()
    s.Add(y <. 11I)
    printfn "%O" s
    printfn "Solving updated set of constraints..."
    printfn "%O" <| s.Check()

    printfn "Restoring state..."
    s.Pop()
    printfn "%O" s
    printfn "Solving restored set of constraints..."
    printfn "%O" <| s.Check()

let solverExample2() =
    let x = Real("x")
    let y = Real("y")
    let s = Solver()
    s.Add(x >. 1.0, y >. 1.0, Or [| x + y >. 3.0; x - y <. 2.0|])
    printfn "asserted constraints..."
    for c in s.Assertions do
        printf "%O" c

    printfn "%O" <| s.Check()
    printfn "statistics for the last check method..."
    printfn "%O" s.Statistics
    // Traversing statistics
    for k, v in s.Statistics do
        printfn "%s : %O" k v

let solverExample3() =
    let x = Real("x")
    let y = Real("y")
    let z = Real("z")
    let s = Solver()
    s.Add(x >. 1.0, y >. 1.0, x + y >. 3.0, z - x <. 10.0)
    printfn "%O" <| s.Check()

    let m = s.Model
    printfn "x = %O" m.[x.Expr]

    printfn "traversing model..."
    for d in m.Decls do
        printfn "%O = %O" <|| (d.Name, m.[d])
    
#time "on";;

let res00 = boolArithExample1();;
let res01 = boolArithExample2();;
let res02 = intArithExample1();;
let res03 = realArithExample1();;
let res04 = mixedArithExample1();;
let res05 = mixedArithExample2();;
let res06 = simplifyExample1();;
let res07 = solverExample1();;
let res08 = solverExample2();;
let res09 = solverExample3();;
