#r "../Microsoft.Z3.dll"
#r "../Z3Fs/bin/Debug/Z3Fs.dll"

open Microsoft.Z3.FSharp
open Common
open Bool
open Int

// Reference from http://rise4fun.com/Z3Py/tutorialcontent/guide#h210

// Create 3 integer variables
let dog = Int("dog")
let cat = Int("cat")
let mouse = Int("mouse")

Z3.Solve(dog >=. 1I,   // at least one dog
         cat >=. 1I,   // at least one cat
         mouse >=. 1I, // at least one mouse
         // we want to buy 100 animals
         dog + cat + mouse =. 100I,  
         // We have 100 dollars (10000 cents):
         // dogs cost 15 dollars (1500 cents), 
         //   cats cost 1 dollar (100 cents), and 
         //   mice cost 25 cents
         1500I * dog + 100I * cat + 25I * mouse =. 10000I)