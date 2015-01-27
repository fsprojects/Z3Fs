#I "../FsZ3/bin/Debug"
#r "Microsoft.Z3"
#r "FsZ3"

open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h210

// We know each queen must be in a different row.
// So, we represent each queen by a single integer: the column position
let queens = [| for i in 0..7 -> Int(sprintf "Q_%i" (i + 1)) |]

// Each queen is in a column {1, 2 ... 8}
let rows = [| for i in 0..7 -> And [|1I <=. queens.[i]; queens.[i] <=. 8I|] |]

// At most one queen per column
let cols = [| Distinct(queens) |]

// Diagonal constraint
let diags = [| for i in 1..7 do
                 for j in 0..i-1 do 
                       yield And [| queens.[i] - queens.[j] <>. bigint (i - j);
                                    queens.[i] - queens.[j] <>. bigint (j - i) |] |];;

Z3.Solve(rows ++ cols ++ diags)