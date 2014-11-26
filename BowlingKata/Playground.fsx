#load "SuperStronglyTyped.fs"

open BowlingKata.SuperStronglyTyped

// TESTING
// =======

let test game expectedScore = 
    let actualScore = scoreGame game
    if actualScore = expectedScore
    then printfn "SUCCESS! %s = %i" game actualScore
    else printfn "FAILURE! %s: Expected %i, Actual %i" game expectedScore actualScore

test "54545454545454545454" 90
test "XXXXXXXXXXXX" 300
test "5/5/5/5/5/5/5/5/5/5/5" 150
test "9-9-9-9-9-9-9-9-9-9-" 90
    
    


