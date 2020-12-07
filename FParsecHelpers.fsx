module FParsecHelpers

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open FParsec

let unwrap_result (parser: ParserResult<'a, 'b>): 'a =
    match parser with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> Unchecked.defaultof<'a>

let get_some_count (target: 'a option list) =
    target
    |> List.filter (fun a -> a.IsSome)
    |> List.length
    |> fun a -> a.ToString()