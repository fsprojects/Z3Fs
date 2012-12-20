#r "../Microsoft.Z3.dll"
#r "../Z3Fs/bin/Debug/Z3Fs.dll"

open Microsoft.Z3
open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

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
        printfn "%O = %O" d.Name m.[d]

#time "on";;

let res01 = solverExample1();;
let res02 = solverExample2();;
let res03 = solverExample3();;
