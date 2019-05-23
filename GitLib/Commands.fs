namespace GitLib

module Commands = 
    open System
    open System.IO
    open System.Text
    open Utils

    let getUnixTime() = 
        DateTimeOffset.UtcNow.ToUnixTimeSeconds()

    let getDefaultUserData() =
        { 
            Name = "Git User";
            Email = "user@git.org";
            Date = { DateSeconds = getUnixTime(); DateTimeZone = "+0100" } 
        }

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

        SymbolicReferences.serializeSymRef "refs/heads/master"
        |> ReferencesStorage.writeReference rootDir "HEAD"

    let hashObject (rootDir: string) (currentDir: string) (relativePath: string) (write: bool) : unit = 
        let objectPath = Path.Combine(currentDir, relativePath)

        let content = File.ReadAllBytes(objectPath)
        let blob = { Content  = content }

        let wrappedObject = 
            blob
            |> Blobs.serializeBlob 
            |> GitObjects.wrap

        let hash = wrappedObject |> Hash.sha1Bytes
    
        if write then 
            Storage.writeObjectContent rootDir hash wrappedObject

        let (Sha1 objectId) = hash
        objectId |> printf "%s"

    let catFiles (rootDir: string) (option: string) (objectId: string) : unit = 
        result {
            let! hash = objectId |> Hash.parse
            let! object = 
                hash
                |> Storage.readObject rootDir 
                |> GitObjects.unwrap
            let! res = 
                match option with 
                | "-t" -> Ok (object.ObjectType |> ObjectTypes.toStr)
                | "-s" -> Ok (object.Size |> sprintf "%i")
                | "-p" -> 
                    match object.ObjectType with
                    | Blob -> object |> Blobs.parseBlob |> Result.map Blobs.formatBlob
                    | Tree -> object |> Trees.parseTree |> Result.map Trees.formatTree
                    | Commit -> object |> Commits.parseCommit |> Result.map Commits.formatCommit
                    | Tag -> object |> Tags.parseTag |> Result.map Tags.formatTag
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
        sprintf "%s %s %i %s\n" (UnixFileModes.toStr mode) id stage path

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
                let! entryMode = UnixFileModes.fromStr mode
                let newEntry = GitIndexEntry.Create fileInfo entryMode objectId fileRelativePath
                return newEntry
            }

            return { Entries = newEntry :: oldIndex.Entries }
        }
        
        match newIndex with 
        | Ok index -> index |> GitIndexes.serializeIndex |> Storage.writeIndex rootDir
        | Error reason -> printf "%s" reason


    let writeTree (rootDir: string) =
        let trees = result {
            let! index = rootDir |> Storage.readIndex |> GitIndexes.parseIndex
            let treeModel = MakeTree.getTree index
            let trees = MakeTree.createTreeObjects treeModel
            
            let objects = 
                trees 
                |> List.map (fun t -> 
                    let bytes = 
                        Trees.serializeTree t
                        |> GitObjects.wrap
                    let hash = Hash.sha1Bytes bytes 
                    (hash, bytes))
            return objects
        }
        match trees with 
        | Ok tlist -> 
            tlist |> List.iter (fun (h, b) -> Storage.writeObjectContent rootDir h b)
        | Error reason -> 
            failwith reason

    let commitTree (rootDir: string) (treeObjectId: string) (parents: string list) (message: string): unit = 
        let userData = getDefaultUserData()

        let newCommit = result {
            let! tree = treeObjectId |> Hash.parse
            let! parentCommits = parents |> List.map Hash.parse |> traverse

            let commit: Commit = {
                Tree = tree
                Parents = parentCommits
                Author = userData
                Commiter = userData
                Message = message
            }

            let commitBytes = 
                commit
                |> Commits.serializeCommit
                |> GitObjects.wrap
            let commitHash = commitBytes |> Hash.sha1Bytes

            return (commitHash, commitBytes)
        }
        match newCommit with 
        | Ok (hash, bytes) -> Storage.writeObjectContent rootDir hash bytes
        | Error reason -> failwith reason


    type UpdateRefArgs = 
    | CreateOrUpdateRef of ref:string * newValue:string
    | UpdateRefSafe of ref:string * newValue:string * oldValue:string
    | DeleteRef of ref:string
    | DeleteRefSafe of ref:string * oldValue:string

    let updateRef (rootDir: string) (args: UpdateRefArgs): unit = 
        match args with
        | DeleteRef ref -> ReferencesStorage.deleteReference rootDir ref
        | DeleteRefSafe(ref, oldValue) ->
            let value = ReferencesStorage.readReference rootDir ref
            if value = oldValue then
                ReferencesStorage.deleteReference rootDir ref
            else
                failwith "ref has different value than specified"
        | CreateOrUpdateRef(ref, newValue) ->
            ReferencesStorage.writeReference rootDir ref newValue
        | UpdateRefSafe(ref, newValue, oldValue) ->
            let value = ReferencesStorage.readReference rootDir ref
            if value = oldValue then
                ReferencesStorage.writeReference rootDir ref newValue
            else
                failwith "ref has different value than specified"

    type SymbolicRefArgs = 
    | CreateSymRef of name:string * ref:string
    | ReadSymRef of name:string
    | DeleteSymRef of name:string

    let symbolicRef (rootDir: string) (args: SymbolicRefArgs): unit = 
        match args with 
        | CreateSymRef(name, ref) ->
            ref
            |> SymbolicReferences.serializeSymRef
            |> ReferencesStorage.writeReference rootDir name 
        | ReadSymRef(name) -> 
            ReferencesStorage.readReference rootDir name
            |> SymbolicReferences.parseSymRef
            |> printf "%s" 
        | DeleteSymRef(name) -> 
            ReferencesStorage.deleteReference rootDir name


    let createTag (rootDir: string) message name objectId = 
        result {
            let! oid = Hash.parse objectId

            let! taggedObject = 
                Storage.readObject rootDir oid 
                |> GitObjects.unwrap 
        
            let tag = 
                {
                    ObjectId = oid
                    TaggedObjectType = taggedObject.ObjectType
                    Name = name
                    TaggerData = getDefaultUserData()
                    TagMessage = message
                }

            let tagBytes = 
                tag
                |> Tags.serializeTag
                |> GitObjects.wrap
            let tagHash = tagBytes |> Hash.sha1Bytes

            Storage.writeObjectContent rootDir tagHash tagBytes

            let (Sha1 objectHash) = tagHash
            ReferencesStorage.writeTagReference rootDir name objectHash
            
            return ()
        } 
        |> (function
            | Ok _ -> ()
            | Error reason -> failwith reason)

    let deleteTag (rootDir: string) tagname = 
        result {
            let! tagHash = 
                ReferencesStorage.readTagReference rootDir tagname
                |> Hash.parse

            Storage.deleteObject rootDir tagHash
            ReferencesStorage.deleteTagReference rootDir tagname

            return ()
        }
        |> (function
            | Ok _ -> ()
            | Error reason -> failwith reason)

    let listTags (rootDir: string) = 
        let tags = ReferencesStorage.readAllTagReferences rootDir
        String.Join("\n", tags) |> printf "%s"
        