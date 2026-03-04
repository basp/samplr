#r "nuget: Plotly.NET"

open System
open Plotly.NET

let pi = Math.PI

/// <summary>
/// Sine takes an input value from -pi to pi
/// and returns a value from -1 to 1.
/// </summary>
let sine x =
    let sineB = 4.0 / pi
    let sineC = -4.0 / (pi * pi)
    let sineP = 0.225
    let y = sineB * x + sineC * x * (abs x)
    sineP * (y * (abs y) - y) + y

/// <summary>
/// Triangle takes an input value from -pi to pi
/// and returns a value from -1 to 1.
/// </summary>
let triangle x =
    let normalized = (x + pi) / (2.0 * pi)
    let t = normalized - float (int normalized)
    if t < 0.5 then 4.0 * t - 1.0
    else 4.0 * (1.0 - t) - 1.0

/// <summary>
/// Square takes an input value from -pi to pi
/// and returns a value from -1 to 1.
/// </summary>
let square x =
    if x >= 0.0 then 1.0
    else -1.0
    
/// <summary>
/// Saw takes an input value from -pi to pi
/// and returns a value from -1 to 1.
/// </summary>
let saw x =
    let sawA = 1.0 / pi
    sawA * x    
   
let sampleRate = 44100.0
let frequency = 8.0
let duration = 2.0
let count = duration * sampleRate |> int      
    
let xs =
    [0 .. count - 1]
    |> List.map (fun i ->
        let t = float i / sampleRate
        let phase = 2.0 * pi * frequency * t
        (phase + pi) % (2.0 * pi) - pi)
    
let sineGraph =
    let ys = xs |> List.map sine
    List.zip xs ys
    
let triangleGraph =
    let ys = xs |> List.map triangle
    List.zip xs ys
    
let squareGraph =
    let ys = xs |> List.map square
    List.zip xs ys
    
let sawGraph =
    let ys = xs |> List.map saw
    List.zip xs ys
    
let charts = [
    sineGraph |> Chart.Line
    triangleGraph |> Chart.Line
    squareGraph |> Chart.Line
    sawGraph |> Chart.Line
]

charts |> Chart.combine |> Chart.show
