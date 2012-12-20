#r "../Microsoft.Z3.dll"
#r "../Z3Fs/bin/Debug/Z3Fs.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Z3
open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h28

/// Ima Hurryin is approaching a stoplight moving with a velocity of 30.0 m/s. 
/// The light turns yellow, and Ima applies the brakes and skids to a stop. 
/// If Ima's acceleration is -8.00 m/s2, then determine the displacement of the car during the skidding process.
let problem1() =
    let d = Real("d")
    let a = Real("a")
    let t = Real("t")
    let v_i = Real("v_i")
    let v_f = Real("v_f")

    let equations = 
        [| d =. v_i * t + (a * t ** 2I) / 2.0;
           v_f =. v_i + a*t |]
    printfn  "Kinematic equations: %A" equations

    // Given v_i, v_f and a, find d
    let problem = 
        [|
            v_i =. 30.0;
            v_f =. 0.0;
            a =. -8.0
        |]
    printfn "Problem: %A" problem 

    printfn "Solution: %A" <| Z3.Solve(equations ++ problem)

/// Ben Rushin is waiting at a stoplight. 
/// When it finally turns green, Ben accelerated from rest at a rate of a 6.00 m/s2 for a time of 4.10 seconds. 
/// Determine the displacement of Ben's car during this time period.
let problem2() =
    let d = Real("d")
    let a = Real("a")
    let t = Real("t")
    let v_i = Real("v_i")
    let v_f = Real("v_f")

    let equations = 
        [| 
           d =. v_i * t + (a * t ** 2I) / 2.0;
           v_f =. v_i + a*t 
        |]
    printfn  "Kinematic equations: %A" equations

    // Given v_i, v_f and a, find d
    let problem = 
        [|
            v_i =. 0.0;
            v_f =. 4.10;
            a =. 6.0
        |]
    printfn "Problem: %A" problem 

    printfn "Solution: %A" <| Z3.Solve(equations ++ problem)


#time "on";;
problem1();;
problem2();;