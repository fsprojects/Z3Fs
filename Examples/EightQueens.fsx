#r "../Microsoft.Z3.dll"
#r "../Z3Fs/bin/Debug/Z3Fs.dll"

open Microsoft.Z3
open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h210

// We know each queen must be in a different row.
// So, we represent each queen by a single integer: the column position
let queens = [| for i in 0..7 -> Int(sprintf "Q_%i" (i + 1)) |]

// Each queen is in a column {1, 2 ... 8}
let rows = [| for i in 0..7 -> And [|1I <=. queens.[i]; queens.[i] <=. 8I|] |]

// At most one queen per column
let cols = [| Distinct(queens) |]

// Diagonal constraint
let diags = [| for i in 0..7 do
                 for j in 0..i do
                    if i <> j then 
                       yield And [| queens.[i] - queens.[j] <>. bigint (i - j);
                                    queens.[i] - queens.[j] <>. bigint (j - i) |] |]

Z3.Solve(rows ++ cols ++ diags)