namespace GitLib

module Commands = 
    open System
    open System.IO
    open System.Text
    open Utils

    let init (rootDir: string) : unit =
        let gitDir = Path.Combine(rootDir, ".git")

        let folders = [
            "hooks";
            "info";
            "objects/info";
            "objects/pack";
            "refs/heads";
            "refs/tags"
        ]
        for folder in folders do
            let path = Path.Combine(gitDir, folder)
            Directory.CreateDirectory(path) |> ignore

        let defaultConfig = @"[core]
    repositoryformatversion = 0
    filemode = false
    bare = false
    logallrefupdates = true
    symlinks = false
    ignorecase = true
    "
        let configPath = Path.Combine(gitDir, "config")
        File.WriteAllText(configPath, defaultConfig, Encoding.UTF8)

        let defaultHead = "ref: refs/heads/master\n"
        let headPath = Path.Combine(gitDir, "HEAD")
        File.WriteAllText(headPath, defaultHead, Encoding.UTF8)


    let hashObject (rootDir: string) (currentDir: string) (relativePath: string) (write: bool) : unit = 
        let objectPath = Path.Combine(currentDir, relativePath)

        let content = File.ReadAllBytes(objectPath)
        let wrappedObject = 
            { ObjectType = Blob; Object = content } 
            |> GitObjects.wrap

        let hash = wrappedObject |> Hash.sha1Bytes
    
        if write then 
            Storage.writeObjectContent rootDir hash wrappedObject

        let (Sha1 objectId) = hash
        objectId |> printf "%s"

    // for now works only with blobs and trees
    let catFiles (rootDir: string) (option: string) (objectId: string) : unit = 
        result {
            let! hash = objectId |> Hash.parse
            let! object = 
                hash
                |> Storage.readObject rootDir 
                |> (fun x -> x.Content) 
                |> GitObjects.unwrap
            let! res = 
                match option with 
                | "-t" -> Ok (object.ObjectType |> ObjectTypes.toStr)
                | "-s" -> Ok (object.Size |> sprintf "%i")
                | "-p" -> 
                    match object.ObjectType with
                    | Blob -> object.Object |> Encoding.UTF8.GetString |> Ok
                    | Tree -> object.Object |> Trees.parseTree |> Result.map Trees.formatTree
                    | Commit -> object.Object |> Commits.parseCommit |> Result.map Commits.formatCommit
                | _ -> Error "incorrect cat-files option"
            return res
        } |> (function 
                | Ok str -> str |> printf "%s"
                | Error err -> printf "%s" err)


    type LsFileFormat = Default | ShowObjectNames

    let private formatEntryShowNames (entry: GitIndexEntry) = 
        let {
            Mode = mode; 
            Hash = Sha1 id; 
            Flags = { Stage = stage }; 
            RelativeFilePath = path 
            } = entry
        sprintf "%s %s %i %s\n" (IndexEntryModes.toStr mode) id stage path

    let private formatEntryDefault ({ RelativeFilePath = path }) =
        sprintf "%s\n" path

    let private formatEntries entries format =
        let formatFun =
            match format with 
            | Default -> formatEntryDefault
            | ShowObjectNames -> formatEntryShowNames
        List.map formatFun entries 

    // ls-files should list files from index and form working tree
    let lsFiles (rootDir: string) (format: LsFileFormat) =
        rootDir
        |> Storage.readIndex 
        |> GitIndexes.parseIndex
        |> Result.map(fun index -> formatEntries index.Entries format)
        |> Result.map (fun xs -> String.Join("", xs) )
        |> (function 
            | Ok str -> str |> printf "%s"
            | Error err -> printf "%s" err)
    

    type CacheInfo = CacheInfo of mode:string * hash:string * path:string

    let updateIndexAdd (rootDir: string) (currentDir: string) (CacheInfo(mode, hash, path)) = 
        let filePath = Path.Combine(currentDir, path)
        let fileInfo = FileInfo(filePath)
        let fileRelativePath = Path.GetRelativePath(rootDir, filePath)

        let newIndex = result {
            let! oldIndex = 
                if Storage.indexExists rootDir then 
                    rootDir |> Storage.readIndex |> GitIndexes.parseIndex
                else
                    Ok GitIndex.Empty

            let! newEntry = result {
                let! objectId = Hash.parse hash
                let! entryMode = IndexEntryModes.fromStr mode
                let newEntry = GitIndexEntry.Create fileInfo entryMode objectId fileRelativePath
                return newEntry
            }

            return { Entries = newEntry :: oldIndex.Entries }
        }
        
        match newIndex with 
        | Ok index -> index |> GitIndexes.serializeIndex |> Storage.writeIndex rootDir
        | Error reason -> printf "%s" reason


    let writeTree (rootDir: string) =
        let index = Storage.readIndex rootDir
        let treeModel = index |> GitIndexes.parseIndex |> Result.map MakeTree.getTree
        ()