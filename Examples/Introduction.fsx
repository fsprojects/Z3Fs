#I "../Z3Fs/bin/Debug"
#r "Microsoft.Z3.dll"
#r "Z3Fs.dll"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference at http://rise4fun.com/Z3Py/tutorialcontent/guide#h20

let introExample1() =
    let x = Int("x")
    let y = Int("y")
    Z3.Solve(x >. 2I, y <. 10I, x + 2I * y =. 7I)

let introExample2() =
    let x = Int("x")
    let y = Int("y")
    printfn "%O" <| Z3.Simplify(x + y + 2I*x + 3I)
    printfn "%O" <| Z3.Simplify(x <. y + x + 2I)
    printfn "%O" <| Z3.Simplify(And [| x + 1I >=. 3I; x ** 2I + x ** 2I + y ** 2I + 2I >=. 5I |])

let introExample3() =
    let x = Int("x")
    let y = Int("y")
    let n = (x + y >=. 3I).Expr
    printfn "num args: %O" n.NumArgs
    printfn "children: %A" n.Args
    printfn "1st child: %O" n.Args.[0]
    printfn "2nd child: %O" n.Args.[1]
    printfn "operator: %O" n.FuncDecl
    printfn "op name: %O" n.FuncDecl.Name

let introExample4() =
    let x = Real("x")
    let y = Real("y")
    Z3.Solve(x ** 2I + y ** 2I >. 3.0, x ** 3I + y <. 5.0)

let introExample5() =
    let x = Real("x")
    Z3.Solve(x >. 4.0, x <. 0.0)

let introExample6() =
    // This is a comment
    let x = Real("x") // comment: creating x
    printfn "%O" <| x**2I + 2.0*x + 2.0 // comment: printing polynomial
    
#time "on";;

let res01 = introExample1();;
let res02 = introExample2();;
let res03 = introExample3();;
let res04 = introExample4();;
let res05 = introExample5();;
let res06 = introExample6();;
