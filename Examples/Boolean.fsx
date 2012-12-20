#r "../Microsoft.Z3.dll"
#r "../Z3Fs/bin/Debug/Z3Fs.dll"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference at http://rise4fun.com/Z3Py/tutorialcontent/guide#h21

let booleanExample1() =
    let p = Bool "p"
    let q = Bool "q"
    let r = Bool "r"
    Z3.Solve(p =>. q, r =. (!. q), (!. p) ||. r)

let booleanExample2() =
    let p = Bool("p")
    let q = Bool("q")
    let r = Bool("r")
    Z3.Solve(Implies(p, q), r =. Not(q), Or [|Not(p); r|])

let booleanExample3() =
    let p = Bool("p")
    let q = Bool("q")
    printfn "%O" <| And [|p; q; True|]
    printfn "%O" <| Z3.Simplify(And [|p; q; True|])
    printfn "%O" <| Z3.Simplify(And[|p; False|])

let booleanExample4() =
    let p = Bool("p")
    let x = Real("x")
    Z3.Solve(Or [|x <. 5.0; x >. 10.0|], Or [|p; x ** 2I =. 2.0|], Not(p))

#time "on";;

let res01 = booleanExample1();;
let res02 = booleanExample2();;
let res03 = booleanExample3();;
let res04 = booleanExample4();;