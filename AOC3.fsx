module AOC3

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open FParsec
open FParsecHelpers

let run_paths (text: string list) line_width paths =
    paths
    |> List.mapi (fun i s ->
        let (x, y) = s

        let mutable count: int64 = (int64)0
        let mutable index_x = 0
        for index_y in 0 .. y .. text.Length - 1 do
            let square = text.[index_y].[index_x]
            if square = '#' then
                count <- count + (int64)1
            index_x <- (index_x + x) % line_width

        count
    )
    |> List.fold (*) ((int64)1)

let run input =
    let line_width =
        run (pstring "" >>. restOfLine false) input
        |> unwrap_result
        |> String.length

    let text: string list =
        run (pstring "" >>. manyTill (restOfLine true) eof) input
        |> unwrap_result

    let part1 = [(3, 1)] |> run_paths text line_width |> fun a -> "P1: " + a.ToString()
    let part2 = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] |> run_paths text line_width |> fun a -> "P2: " + a.ToString()

    part1 + "\n" + part2