#I "../FsZ3/bin/Debug"
#r "Microsoft.Z3"
#r "FsZ3"

open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Function

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h25

let functionExample1() =
    let x = Int("x")
    let y = Int("y")

    let f (a: Int) = Z3.CreateFunction<Int>("f", IntSort(), a)
    
    Z3.Solve(f(f(x)) =. x, f(x) =. y, x <>. y)

let functionExample2() =
    let x = Int("x")
    let y = Int("y")
    
    let f (x: Int) = Z3.CreateFunction<Int>("f", IntSort(), x)
    
    let s = Solver()
    s.Add(f(f(x)) =. x, f(x) =. y, x <>. y)
    
    printfn "%O" <| s.Check()
    
    let m = s.Model
    printfn "f(f(x)) = %O" <| m.Evaluate(f(f(x)))
    printfn "f(x)    = %O" <| m.Evaluate(f(x))

let functionExample3() =
    let x = Int("x")
    let y = Int("y")
    let c = IntVal(10I)

    let f (a: Int) = Z3.CreateFunction<Int>("f", IntSort(), a)
    
    Z3.Solve(x >. 20I, y >. x, f(c) =. 1I)

#time "on";;

let res01 = functionExample1();;
let res02 = functionExample2();;
let res03 = functionExample3();;