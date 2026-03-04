#r "nuget: Plotly.NET"

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Plotly.NET

type [<Measure>] rad

let pi = Math.PI |> LanguagePrimitives.FloatWithMeasure<rad>
let pi2 = 2.0 * pi
let negPi = -pi

type Waveform =
    | Sine
    | Square
    | Triangle
    | Sawtooth
    | Custom of (float<rad> -> float)

type ISignal =
    abstract member Sample: unit -> float 

type Osc(fs: float<1/s>) =
    let mutable waveform = Sine
    let mutable frequency = 1.0<Hz>    
    // Invariant: currentPhase is in range [-pi, pi).
    let mutable currentPhase = 0.0<rad>
    // Note that float<Hz rad s> = float<1/s rad s> = float<rad>.
    // In the above, the Hz (1/s) and s units cancel out.
    let mutable phaseIncrement = (frequency * pi2) / fs        
        
    let sine (phase: float<rad>) =
        phase |> float |> sin    
    
    let square (phase: float<rad>) =
        if phase < 0.0<rad> then -1.0 else 1.0    
    
    let saw (phase: float<rad>) =
        phase / pi    
    
    let triangle (phase: float<rad>) =
        let x = phase / 1.0<rad>
        1.0 - (2.0 / Math.PI) * abs x
    
    member _.Frequency
        with get () = frequency
        and set value =
            frequency <- value
            phaseIncrement <- (value * pi2) / fs
            
    member _.Waveform
        with get () = waveform
        and set value  =
            waveform <- value
            
    // Resets the oscillator back to its starting state.
    member _.Reset() =
        phaseIncrement <- (frequency * pi2) / fs
        currentPhase <- 0.0<rad>
    
    member this.Sample() = (this :> ISignal).Sample()
    
    interface ISignal with
        member _.Sample() =
            let sample =
                match waveform with
                | Sine -> sine currentPhase
                | Square -> square currentPhase
                | Triangle -> triangle currentPhase
                | Sawtooth -> saw currentPhase
                | Custom f -> f currentPhase
            currentPhase <-
                let next = currentPhase + phaseIncrement
                // Wrap around to keep phase in range of [-pi, pi).
                if next >= pi then next - pi2
                elif next < negPi then next + pi2
                else next
            sample

let sampleRate = 128.0<1/s>; 

let modulator = Osc sampleRate
modulator.Frequency <- 2.0<Hz>

let carrier = Osc sampleRate

/// <summary>
/// Modulates a carrier signal with a modulator signal.
/// </summary>
/// <param name="carrier">The carrier signal.</param>
/// <param name="cf">The carrier base frequency (in Hz).</param>
/// <param name="md">The modulation depth (in Hz).</param>
/// <param name="modulator">The modulation signal.</param>
let modulate
    (carrier: Osc)
    (cf: float<Hz>) 
    (md: float<Hz>)
    (modulator: ISignal) =
    { new ISignal with
        member _.Sample() =
            // f(t) = cf<Hz> + md<Hz> * sin(2π * mf<Hz> * t<s>)
            let modulation = modulator.Sample() * md
            carrier.Frequency <- cf + modulation
            carrier.Sample() }

let fm =
   modulator
   |> modulate carrier 8.0<Hz> 2.5<Hz>

let add (config: (float * Osc) list) =
    { new ISignal with
        member _.Sample() =
            config
            |> List.fold (fun acc (weight, osc) ->
                acc + weight * osc.Sample()) 0.0}

let additive =
    let osc1 = Osc sampleRate
    osc1.Frequency <- 1.0<Hz>
    osc1.Waveform <- Square
    let osc2 = Osc sampleRate
    osc2.Frequency <- 1.33<Hz>
    add [(0.7, osc1); (0.3, osc2)]

let xs =
    List.init
        (int (sampleRate * 3.0<s>))
        (fun i -> float i / sampleRate)
        
let charts = [
    // xs
    // |> List.map (fun _ -> modulator.Sample())
    // |> List.zip xs
    // |> Chart.Line
    //
    // xs
    // |> List.map (fun _ -> fm.Sample())
    // |> List.zip xs
    // |> Chart.Line
    
    xs
    |> List.map (fun _ -> additive.Sample())
    |> List.zip xs
    |> Chart.Line   
]

charts
|> Chart.combine
|> Chart.show
