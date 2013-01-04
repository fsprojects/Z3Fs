#I "../Z3Fs/bin/Debug"
#r "Microsoft.Z3.dll"
#r "Z3Fs.dll"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h23

let arithExample1() =
    let a = Int("a")
    let b = Int("b")
    let c = Int("c")
    let d = Real("d")
    let e = Real("e")
    Z3.Solve(a >. b + 2I, a =. 2I * c + 10I, c + b <=. 1000I, d >=. e)

let arithExample2() =
    let x = Real("x")
    let y = Real("y")
    // Put expression in sum-of-monomials form
    let t = Z3.Simplify((x + y) ** 3I, ":som" => true)
    printfn "%O" t
    // Use power operator
    let t = Z3.Simplify(t, ":mul-to-power" => true)
    printfn "%O" t

let arithExample3() =
    let x = Real("x")
    let y = Real("y")
    // Using Z3 native option names
    printfn "%O" <| Z3.Simplify(x =. y + 2.0, ":arith-lhs" => true)

#time "on";;

let res01 = arithExample1();;
let res02 = arithExample2();;
let res03 = arithExample3();;