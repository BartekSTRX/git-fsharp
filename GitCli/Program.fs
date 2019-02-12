open GitLib
open GitLib.Commands

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
    // git mktree -- creates a tree from stdin
    // git ls-tree -- seems to work exactly like hash-object -p for trees
    // read-tree --prefix=bak
    // commit-tree
    // ls-tree
    // branch?
    // update-ref
    // symbolic-ref
    // log
    // git add => git hash-object -w && git update-index
    // git ls-tree VS git cat-file -p <hash of a tree>

    //let dir = Directory.GetCurrentDirectory()
    let dir = "C:\Users\LAPPEK4\Documents\heh2"
    
    //Commands.lsFiles dir LsFileFormat.Default
    //Commands.updateIndexAdd dir dir (CacheInfo("100644", "8d5c3f86d71f9d9265b5b47a3b019cfed9cc46a7", "ggggf.txt"))

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
        Commands.writeTree dir
    | ["update-ref"; "-d"; ref] ->
        Commands.updateRef dir (Delete ref)
    | ["update-ref"; ref; newValue] ->
        Commands.updateRef dir (CreateOrUpdate(ref, newValue))
    | ["update-ref"; "-d"; ref; oldValue] ->
        Commands.updateRef dir (DeleteSafe(ref, oldValue))
    | ["update-ref"; ref; newValue; oldValue] ->
        Commands.updateRef dir (UpdateSafe(ref, newValue, oldValue))
    | ["symbolic-ref"; "-d"; name] ->
        Commands.symbolifRef dir (Delete(name))
    | ["symbolic-ref"; name; ref] ->
        Commands.symbolifRef dir (Create(name, ref))
    | ["symbolic-ref"; name] ->
        Commands.symbolifRef dir (Read(name))
    | _ -> printf "incorrect args %A" argv

    0
