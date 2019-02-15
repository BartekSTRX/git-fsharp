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
    // commit-tree
    // update-ref
    // symbolic-ref

    // TODO
    // tag -- create tags
    // show-ref --- display object id tag refers to
    // git mktree -- creates a tree from stdin
    // git ls-tree -- seems to work exactly like hash-object -p for trees
    // read-tree --prefix=bak
    // ls-tree
    // branch?
    // log
    // git add => git hash-object -w && git update-index
    // git ls-tree VS git cat-file -p <hash of a tree>

    //let dir = Directory.GetCurrentDirectory()
    let dir = "C:\Users\LAPPEK4\Documents\heh2"
    
    //Commands.createTag dir "olololoo" "555" "20dddaf1ebb41118f3fc9bd960d34ccf5257e6db"
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
    | ["tag"; "-a"; "-m"; message; name; objectId] ->
        Commands.createTag dir message name objectId
    | ["update-ref"; "-d"; ref] ->
        Commands.updateRef dir (DeleteRef ref)
    | ["update-ref"; ref; newValue] ->
        Commands.updateRef dir (CreateOrUpdateRef(ref, newValue))
    | ["update-ref"; "-d"; ref; oldValue] ->
        Commands.updateRef dir (DeleteRefSafe(ref, oldValue))
    | ["update-ref"; ref; newValue; oldValue] ->
        Commands.updateRef dir (UpdateRefSafe(ref, newValue, oldValue))
    | ["symbolic-ref"; "-d"; name] ->
        Commands.symbolicRef dir (DeleteSymRef(name))
    | ["symbolic-ref"; name; ref] ->
        Commands.symbolicRef dir (CreateSymRef(name, ref))
    | ["symbolic-ref"; name] ->
        Commands.symbolicRef dir (ReadSymRef(name))
    | _ -> printf "incorrect args %A" argv

    0
