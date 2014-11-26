BowlingKata
===========

Hanging out at BuildStuff 2014, @ToJans and I were attempting to do the Bowling Kata in F# using types to make illegal states unrepresentable.

He'd been to the Idris talk about dependent types (which I missed), but anyway I had a shot at doing something naive using the type system in F# (which doesn't support dependent types).

Here's the model I ended up with:

```
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
```

