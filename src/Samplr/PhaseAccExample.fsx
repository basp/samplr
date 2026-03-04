#r "nuget: Plotly.NET"

open System
open Plotly.NET

let pi = Math.PI
let negPi = -pi
let twoPi = 2.0 * pi

let sampler freq fs =
    let incr = (freq * twoPi) / fs
    let mutable angle = 0.0
    fun () ->
        let prev = angle
        angle <-
            let next = angle + incr            
            if next > pi then next - twoPi
            elif next <= negPi then next + twoPi
            else next
        prev
        
// Sample rate - if we sample exactly 256 times, then we have 1s of output.
let fs = 256.0
let s = sampler 1 fs
let dur = 2.0
let n = int (dur * fs)


let ts = [0..n] |> List.map (fun _ -> s())
let xs = ts |> List.mapi (fun i _ -> float i / float fs)
let ys = ts |> List.map sin

let charts = [
    ts |> List.map sin |> List.zip xs |> Chart.Line
    ts |> List.map cos |> List.zip xs |> Chart.Line
]

Chart.combine charts |> Chart.show