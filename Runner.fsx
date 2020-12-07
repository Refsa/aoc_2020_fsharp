module Runner

open System.IO

let runAOCDay day runner =

    File.ReadAllText("inputs/aoc_" + day.ToString() + "_input.txt")
    |> runner
    |> printfn "%s"

    ()
