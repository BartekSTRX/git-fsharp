open System
open GitLib
open System.IO

[<EntryPoint>]
let main argv =
    let dir = Directory.GetCurrentDirectory()

    Commands.hashObject dir "GitCli.runtimeconfig.json"

    match argv with 
    | [| "init" |] -> Commands.init(dir)
    | [| "hash-object"; relativePath |] -> Commands.hashObject dir relativePath
    | _ -> printf "incorrect args %A" argv

    0
