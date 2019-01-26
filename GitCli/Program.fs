open System
open GitLib.Models

[<EntryPoint>]
let main argv =
    
    //match argv with 
    //| [| "init" |] -> Commands.init()
    //| _ -> printf "incorrect args %A" argv

    let objectId = "6152b602d2550fc550fbed64d8619a59ecee2af6"
    let gitDir = @"C:\Users\LAPPEK4\Documents\GitFsharp\.git"

    let fileContent = Storage.readObject gitDir objectId ObjectFormat.Deflated

    let unwrappedObject = GitObjects.unwrap fileContent.Content

    0
