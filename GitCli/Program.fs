open GitLib
open GitLib.Commands
open System

[<EntryPoint>]
let main argv =

    // Implemented
    // init -- create new repository
    // hash-object -- calculate hash of a file
    // hash-object -w -- save file as blob in /objects
    // cat-files -- given hash, returns type, size or content of an object // TODO commits and tags
    // ls-files -- display content of the index // TODO more options, display content of the working-tree
    // update-index -- add a file to the index

    // TODO
    // write-tree
    // read-tree --prefix=bak
    // commit-tree
    // ls-tree
    // branch?
    // update-ref
    // symbolic-ref
    // log
    // git add => git hash-object -w && git update-index

    //let dir = Directory.GetCurrentDirectory()
    let dir = "C:\Users\LAPPEK4\Documents\heh2"

    Commands.catFiles dir "-p" "504b00c3b4fe52270904541cb6dd84cbd2a67e02"

    //Commands.lsFiles dir LsFileFormat.Default
    //Commands.updateIndexAdd dir dir (CacheInfo("100644", "8d5c3f86d71f9d9265b5b47a3b019cfed9cc46a7", "ggggf.txt"))

    Console.ReadLine() |> ignore

    let argsList = argv |> List.ofArray

    match argsList with 
    | ["init"] -> 
        Commands.init(dir)
    | ["hash-object"; relativePath] -> 
        Commands.hashObject dir dir relativePath false
    | ["hash-object"; "-w"; relativePath] -> 
        Commands.hashObject dir dir relativePath true
    | ["cat-file"; option; hash] ->
        Commands.catFiles dir option hash
    | ["ls-files"] ->
        Commands.lsFiles dir LsFileFormat.Default
    | ["ls-files"; "-s"] ->
        Commands.lsFiles dir LsFileFormat.ShowObjectNames
    | ["update-index"; "--add"; "--cacheinfo"; mode; hash; filePath] ->
        Commands.updateIndexAdd dir dir (CacheInfo(mode, hash, filePath))
    | ["write-tree"] ->
        ()
    | _ -> printf "incorrect args %A" argv

    0
