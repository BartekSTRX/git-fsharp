namespace GitLib


type TreeEntryType = BlobEntry | SubTreeEntry

type TreeEntry = TreeEntry of IndexEntryMode * Sha1 * path:string
with 
    member this.EntryType = 
        match this with
        | TreeEntry(Mode040000, _, _) -> SubTreeEntry
        | _ -> BlobEntry

type Tree = {
    TreeEntries: TreeEntry list
}

module Trees =
    open System
    open System.Text

    let parseTree (content: byte[]) : Result<Tree, string> = 
        let rec internalParse (bytes: byte[]) (parsed: TreeEntry list): Result<TreeEntry list, string> =
            match bytes with
            | [| |] -> Ok parsed
            | _ -> 
                let spaceChar = Convert.ToByte(' ')
                let firstSpace = Array.IndexOf(bytes, spaceChar)
                let nullChar = Convert.ToChar(0) |> Convert.ToByte
                let firstNull = Array.IndexOf(bytes, nullChar)

                let encodedMode = 
                    Array.sub bytes 0 firstSpace
                    |> Encoding.UTF8.GetString
                    |> IndexEntryModes.parse
                let encodedPath = 
                    Array.sub bytes (firstSpace + 1)  (firstNull - (firstSpace + 1))
                    |> Encoding.UTF8.GetString
                let hash = 
                    Array.sub bytes (firstNull + 1) 20
                    |> Hash.fromByteArray
                let endOfEntry = (firstNull + 1 + 20)
                let remainingBytes = Array.sub bytes endOfEntry (bytes.Length - endOfEntry)

                match encodedMode with 
                | Ok mode -> 
                    let newEntry = TreeEntry(mode, hash, encodedPath)
                    internalParse remainingBytes (newEntry :: parsed)
                | Error reason -> Error reason
        let result = internalParse content []
        match result with 
        | Ok entries -> Ok { TreeEntries = (entries |> List.rev) }
        | Error reason -> Error reason

    let serializeTree ({ TreeEntries = entries }: Tree): byte[] =
        let serializeEntry (TreeEntry(mode, hash, path)) = 
            let modeAndPathBytes = 
                ((IndexEntryModes.toStr mode), path, Convert.ToChar(0)) 
                |||> sprintf "%s %s%c"
                |> Encoding.UTF8.GetBytes
            let hashBytes = hash |> Hash.toByteArray
            Array.concat [modeAndPathBytes; hashBytes]
        entries
        |> Seq.ofList
        |> Seq.map serializeEntry
        |> Seq.concat
        |> Seq.toArray


    let formatTree ({ TreeEntries = entries }: Tree) : string = 
        let modeToEntryType = function
            | Mode100644 | Mode100755 | Mode120000 -> "blob"
            | Mode040000 -> "tree"
        let formatEntry (TreeEntry(mode, Sha1 hash, path)) =
            let modeStr = mode |> IndexEntryModes.toStr
            let typeStr = mode |> modeToEntryType
            sprintf "%s %s %s\t%s" modeStr typeStr hash path
        let formatedEntries = 
            entries 
            |> List.sortBy (fun (TreeEntry(_, _, path)) -> path)
            |> List.map formatEntry
        String.Join("\n", formatedEntries)
