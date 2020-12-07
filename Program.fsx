// Learn more about F# at http://fsharp.org

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open System

[<EntryPoint>]
let main argv =
    // Runner.runAOCDay 1 AOC1.run
    // Runner.runAOCDay 2 AOC2.run
    Runner.runAOCDay 3 AOC3.run
    0
