module BowlingKata.SuperStronglyTyped

open System

// THE MODEL represents only allowed states
// ========================================
type Game =
    { F1 : Frame; F2 : Frame; F3 : Frame; F4 : Frame; F5 : Frame; F6 : Frame; F7 : Frame; F8 : Frame; F9 : Frame; F10 : FinalFrame } 

and Frame = 
    | Strike
    | Spare of PinCount 
    | Pins of PinCombo

// Separate type for final frame because it is special, e.g. only have bonus rolls when it's a strike
and FinalFrame =
    | Strike of StrikeBonusRolls
    | Spare of PinCount * SpareBonusRoll
    | Pins of PinCombo

and StrikeBonusRolls = 
    | TwoStrikes
    | BonusSpare
    | BonusPins of PinCombo

and SpareBonusRoll = 
    | BonusPins of PinCount
    | BonusStrike

and PinCount = P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9

// All the allowable combinations of pins that are not spares
and PinCombo = 
    | P0_0 | P0_1 | P0_2 | P0_3 | P0_4 | P0_5 | P0_6
    | P0_7 | P0_8 | P0_9 | P1_0 | P1_1 | P1_2 | P1_3
    | P1_4 | P1_5 | P1_6 | P1_7 | P1_8 | P2_0 | P2_1
    | P2_2 | P2_3 | P2_4 | P2_5 | P2_6 | P2_7 | P3_0
    | P3_1 | P3_2 | P3_3 | P3_4 | P3_5 | P3_6 | P4_0
    | P4_1 | P4_2 | P4_3 | P4_4 | P4_5 | P5_0 | P5_1
    | P5_2 | P5_3 | P5_4 | P6_0 | P6_1 | P6_2 | P6_3
    | P7_0 | P7_1 | P7_2 | P8_0 | P8_1 | P9_0

// CODEGEN for combos
//let combos = 
//    seq {
//        for x in [0..9] do
//            for y in [0..9] do
//                if x + y < 10 then
//                    yield sprintf "| (P%i,P%i) -> P%i_%i" x y x y
//    } |> Seq.toList

// SCORING the game
// ================
let comboToPins = function
    | P0_0 -> (P0,P0) | P0_1 -> (P0,P1) | P0_2 -> (P0,P2) | P0_3 -> (P0,P3)
    | P0_4 -> (P0,P4) | P0_5 -> (P0,P5) | P0_6 -> (P0,P6) | P0_7 -> (P0,P7)
    | P0_8 -> (P0,P8) | P0_9 -> (P0,P9) | P1_0 -> (P1,P0) | P1_1 -> (P1,P1)
    | P1_2 -> (P1,P2) | P1_3 -> (P1,P3) | P1_4 -> (P1,P4) | P1_5 -> (P1,P5)
    | P1_6 -> (P1,P6) | P1_7 -> (P1,P7) | P1_8 -> (P1,P8) | P2_0 -> (P2,P0)
    | P2_1 -> (P2,P1) | P2_2 -> (P2,P2) | P2_3 -> (P2,P3) | P2_4 -> (P2,P4)
    | P2_5 -> (P2,P5) | P2_6 -> (P2,P6) | P2_7 -> (P2,P7) | P3_0 -> (P3,P0)
    | P3_1 -> (P3,P1) | P3_2 -> (P3,P2) | P3_3 -> (P3,P3) | P3_4 -> (P3,P4)
    | P3_5 -> (P3,P5) | P3_6 -> (P3,P6) | P4_0 -> (P4,P0) | P4_1 -> (P4,P1)
    | P4_2 -> (P4,P2) | P4_3 -> (P4,P3) | P4_4 -> (P4,P4) | P4_5 -> (P4,P5)
    | P5_0 -> (P5,P0) | P5_1 -> (P5,P1) | P5_2 -> (P5,P2) | P5_3 -> (P5,P3)
    | P5_4 -> (P5,P4) | P6_0 -> (P6,P0) | P6_1 -> (P6,P1) | P6_2 -> (P6,P2)
    | P6_3 -> (P6,P3) | P7_0 -> (P7,P0) | P7_1 -> (P7,P1) | P7_2 -> (P7,P2)
    | P8_0 -> (P8,P0) | P8_1 -> (P8,P1) | P9_0 -> (P9,P0)

let score game = 
    let pins = function P0 -> 0 | P1 -> 1 | P2 -> 2 | P3 -> 3 | P4 -> 4 | P5 -> 5 | P6 -> 6 | P7 -> 7 | P8 -> 8 | P9 -> 9

    let comboSum combo = 
        let (p1,p2) = comboToPins combo
        (p1 |> pins) + (p2 |> pins)

    let pinCount frame =
        let score = 
            match frame with
            | Frame.Strike -> 10
            | Frame.Spare _ -> 10
            | Frame.Pins combo -> combo |> comboSum
        score

    let oneRollBonus frame =
        match frame with 
        | Frame.Strike -> 10 
        | Frame.Spare p -> p |> pins
        | Frame.Pins combo -> combo |> comboToPins |> fst |> pins

    let scoreFrame (score,prev2Frame,prevFrame) (frame : Frame) =     
        let newScore = 
            let pins = frame |> pinCount
            let strikeRoll2Bonus = 
                match prev2Frame with
                | Some p2f -> match p2f with | Frame.Strike -> oneRollBonus frame | _ -> 0
                | None -> 0
            let lastFrameBonus = 
                match prevFrame with
                | Some pf ->
                    match pf with
                    | Frame.Strike  -> pins
                    | Frame.Spare _ -> oneRollBonus frame
                    | Frame.Pins _  -> 0
                | None -> 0
            score + pins + strikeRoll2Bonus + lastFrameBonus
        
        newScore,prevFrame,(Some frame)

    let scoreFinalFrame (score,prev2Frame,prevFrame) frame =
        let normalFrame,finalFrameScore = 
            match frame with
            | FinalFrame.Strike bonusRolls -> 
                let strikeBonus = 
                    match bonusRolls with
                    | TwoStrikes -> 20
                    | BonusSpare -> 10
                    | StrikeBonusRolls.BonusPins combo -> combo |> comboSum
                Frame.Strike,10 + strikeBonus
            | FinalFrame.Spare (p,bonusRoll) ->
                let spareBonus = 
                    match bonusRoll with
                    | SpareBonusRoll.BonusPins b -> b |> pins
                    | SpareBonusRoll.BonusStrike -> 10
                Frame.Spare(p),10 + spareBonus
            | FinalFrame.Pins combo ->
                Frame.Pins(combo),0
        
        let frameScore,_,_ = scoreFrame (score,prev2Frame,prevFrame) normalFrame
        frameScore + finalFrameScore

    let (>+) = scoreFrame
    let (|+) = scoreFinalFrame

    (0,None,None) >+ game.F1 >+ game.F2 >+ game.F3 >+ game.F4 >+ game.F5 >+ game.F6 >+ game.F7 >+ game.F8 >+ game.F9 |+ game.F10

// PARSING string input 
// ====================
let (|StrikeChar|SpareChar|PinsChar|) (c : char) =
    match c with
    | 'X' -> StrikeChar
    | '/' -> SpareChar
    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> PinsChar (Int32.Parse <| string c) 
    | x -> failwithf "Expected 'X' or '/' or '1-9', found %c" x // todo should this be in active pattern?

let toPinCount pins = match pins with 0 -> P0 | 1 -> P1 | 2 -> P2 | 3 -> P3 | 4 -> P4 | 5 -> P5 | 6 -> P6 | 7 -> P7 | 8 -> P8 | 9 -> P9 | x -> failwithf "Expected 1-9 pins, sctual %i" x

let toPinCombo p1 p2 = 
   let pins = (p1 |> toPinCount, p2 |> toPinCount)
   match pins with
   | (P0,P0) -> P0_0 | (P0,P1) -> P0_1 | (P0,P2) -> P0_2
   | (P0,P3) -> P0_3 | (P0,P4) -> P0_4 | (P0,P5) -> P0_5
   | (P0,P6) -> P0_6 | (P0,P7) -> P0_7 | (P0,P8) -> P0_8
   | (P0,P9) -> P0_9 | (P1,P0) -> P1_0 | (P1,P1) -> P1_1
   | (P1,P2) -> P1_2 | (P1,P3) -> P1_3 | (P1,P4) -> P1_4
   | (P1,P5) -> P1_5 | (P1,P6) -> P1_6 | (P1,P7) -> P1_7
   | (P1,P8) -> P1_8 | (P2,P0) -> P2_0 | (P2,P1) -> P2_1
   | (P2,P2) -> P2_2 | (P2,P3) -> P2_3 | (P2,P4) -> P2_4
   | (P2,P5) -> P2_5 | (P2,P6) -> P2_6 | (P2,P7) -> P2_7
   | (P3,P0) -> P3_0 | (P3,P1) -> P3_1 | (P3,P2) -> P3_2
   | (P3,P3) -> P3_3 | (P3,P4) -> P3_4 | (P3,P5) -> P3_5
   | (P3,P6) -> P3_6 | (P4,P0) -> P4_0 | (P4,P1) -> P4_1
   | (P4,P2) -> P4_2 | (P4,P3) -> P4_3 | (P4,P4) -> P4_4
   | (P4,P5) -> P4_5 | (P5,P0) -> P5_0 | (P5,P1) -> P5_1
   | (P5,P2) -> P5_2 | (P5,P3) -> P5_3 | (P5,P4) -> P5_4
   | (P6,P0) -> P6_0 | (P6,P1) -> P6_1 | (P6,P2) -> P6_2
   | (P6,P3) -> P6_3 | (P7,P0) -> P7_0 | (P7,P1) -> P7_1
   | (P7,P2) -> P7_2 | (P8,P0) -> P8_0 | (P8,P1) -> P8_1
   | (P9,P0) -> P9_0
   | x -> failwithf "Invalid pin combo %A" x

let parseGame (game : string) =
    let scores = game.ToCharArray()
    let getFrame i =
        match scores.[i] with
        | StrikeChar -> Frame.Strike,i+1
        | PinsChar p1 ->
            match scores.[i + 1] with
            | PinsChar p2 -> 
                let combo = toPinCombo p1 p2
                Frame.Pins(combo), i+2
            | SpareChar -> Frame.Spare (p1 |> toPinCount), i+2
            | StrikeChar -> failwith "Cannot throw a strike with the second ball"
        | SpareChar -> failwith "Cannot throw a spare with the first ball"

    let getFinalFrame i =
        let remaining = scores |> Seq.skip i |> Seq.toList
        match remaining with
        | [StrikeChar;   StrikeChar;   StrikeChar]       -> FinalFrame.Strike(StrikeBonusRolls.TwoStrikes)
        | [StrikeChar;   PinsChar(_);  SpareChar]        -> FinalFrame.Strike(StrikeBonusRolls.BonusSpare)
        | [StrikeChar;   PinsChar(p1); PinsChar(p2)]     -> FinalFrame.Strike(StrikeBonusRolls.BonusPins(toPinCombo p1 p2))
        | [PinsChar(p);  SpareChar;    PinsChar(bonus)]  -> FinalFrame.Spare(p |> toPinCount, SpareBonusRoll.BonusPins(bonus |> toPinCount))
        | [PinsChar(p);  SpareChar;    StrikeChar]       -> FinalFrame.Spare(p |> toPinCount, SpareBonusRoll.BonusStrike)
        | [PinsChar(p1); PinsChar(p2)]                   -> FinalFrame.Pins(toPinCombo p1 p2)
        | x                                              -> failwithf "Invalid final frame rolls: %A" x
        
    let f1,p2 = getFrame 0
    let f2,p3 = getFrame p2
    let f3,p4 = getFrame p3
    let f4,p5 = getFrame p4
    let f5,p6 = getFrame p5
    let f6,p7 = getFrame p6
    let f7,p8 = getFrame p7
    let f8,p9 = getFrame p8
    let f9,p10 = getFrame p9
    let f10 = getFinalFrame p10

    { F1 = f1; F2 = f2; F3 = f3; F4 = f4; F5 = f5; F6 = f6; F7 = f7; F8 = f8; F9 = f9; F10 = f10 }

let scoreGame = parseGame >> score