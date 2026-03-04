open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

type [<Measure>] rad

let freq = 4.0<Hz>

// Sample rate.
let fs = 32.0<Hz>

let pi = Math.PI |> LanguagePrimitives.FloatWithMeasure<rad>
let pi2 = 2.0 * pi
let negPi = -pi

// Basically, we want to know how fast to move through the waveform.
// For this we need to know the frequency and the sample rate.
let incr = (freq * pi2) / fs



