#I "../Z3Fs/bin/Debug"
#r "Microsoft.Z3"
#r "Z3Fs"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Microsoft.Z3.FSharp.Array

// Reference from http://rise4fun.com/Z3Py/AAD

let arrayExample1() =
    let vals = [(IntVal 0I, 10I); (IntVal 1I, 23I); (IntVal 2I, 27I); (IntVal 3I, 12I); 
                (IntVal 4I, 19I); (IntVal 5I, 31I); (IntVal 6I, 41I); (IntVal 7I, 7I)]
    let a = Array<Int, Int>("a", IntSort(), IntSort())
    Z3.Solve(And [|for (i, v) in vals -> Select a i =. v|])

#time "on";;

let res01 = arrayExample1();;