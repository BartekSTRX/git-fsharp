open GitLib

[<EntryPoint>]
let main argv =
    //let dir = Directory.GetCurrentDirectory()

    let dir = @"C:\Users\LAPPEK4\Documents\heh2"
    //let filename = "aaa.txt"

    //let hash = Commands.hashObject dir filename

    match argv with 
    | [| "init" |] -> 
        Commands.init(dir)
    | [| "hash-object"; relativePath |] -> 
        Commands.hashObject dir relativePath
        |> printf "%s"
    | _ -> printf "incorrect args %A" argv

    0
