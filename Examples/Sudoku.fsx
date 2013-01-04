#I "../Z3Fs/bin/Debug"
#r "Microsoft.Z3.dll"
#r "Z3Fs.dll"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int
open Real

// Reference at http://rise4fun.com/Z3Py/tutorialcontent/guide#h210

// 9x9 matrix of integer variables
let X = [| for i in 0..8 ->
             [| for j in 0..8 do 
                    yield Int(sprintf "x_%i_%i" (i+1) (j+1)) |] |]

// each cell contains a value in {1, ..., 9}
let cells = [| for i in 0..8 do
                 for j in 0..8 do
                    yield And [| 1I <=. X.[i].[j]; X.[i].[j] <=. 9I |] |]

// each row contains a digit at most once
let rows = [| for i in 0..8 -> Distinct(X.[i]) |]

// each column contains a digit at most once
let cols = [| for j in 0..8 ->
                Distinct([| for i in 0..8 -> X.[i].[j] |]) |]

// each 3x3 square contains a digit at most once
let squares = 
    [| for i0 in 0..2 do
         for j0 in 0..2 do
            yield Distinct([| for i in 0..2 do
                                for j in 0..2 do
                                    yield X.[3*i0 + i].[3*j0 + j] |]) |]

let sudoku = cells ++ rows ++ cols ++ squares

// sudoku instance, we use '0' for empty cells
let init = 
    [| [|0; 0; 0; 0; 9; 4; 0; 3; 0|]; 
       [|0; 0; 0; 5; 1; 0; 0; 0; 7|]; 
       [|0; 8; 9; 0; 0; 0; 0; 4; 0|]; 
       [|0; 0; 0; 0; 0; 0; 2; 0; 8|]; 
       [|0; 6; 0; 2; 0; 1; 0; 5; 0|]; 
       [|1; 0; 2; 0; 0; 0; 0; 0; 0|]; 
       [|0; 7; 0; 0; 0; 0; 5; 2; 0|]; 
       [|9; 0; 0; 0; 6; 5; 0; 0; 0|]; 
       [|0; 4; 0; 9; 7; 0; 0; 0; 0|] |]

let instance =
    [| for i in 0..8 do
        for j in 0..8 do
            if init.[i].[j] <> 0 then
                yield X.[i].[j] =. bigint init.[i].[j] |];;

Z3.Solve(sudoku ++ instance)