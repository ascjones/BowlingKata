
let score rolls =
    let rec scoreFrame xs frame =
        match xs with
        | _ when frame = 10             -> 0
        | 10::y::z::tail                -> 10 + y + z   + scoreFrame (y::z::tail)   (frame + 1)
        | x::y::z::tail when x + y = 10 -> 10 + z       + scoreFrame (z::tail)      (frame + 1)
        | x::y::rest                    -> x + y        + scoreFrame rest           (frame + 1)
        | _                             -> 0
    scoreFrame rolls 0

score  [10;10;10;10;10;10;10;10;10;10;10;10]
score  [5;4;5;4;5;4;5;4;5;4;5;4;5;4;5;4;5;4;5;4]



