namespace GitLib

type ObjectType = Blob | Tree | Commit

module ObjectTypes =
    let toStr = function
        | Blob -> "blob"
        | Tree -> "tree"
        | Commit -> "commit"

    let fromStr = function
        | "blob" -> Ok Blob
        | "tree" -> Ok Tree
        | "commit" -> Ok Commit
        | _ -> Error "incorrect git object type"


type GitObject = {
    ObjectType: ObjectType
    Object: byte array
}
with 
    member this.Size = this.Object.Length


module GitObjects =
    open System
    open ObjectTypes
    open System.Text

    let wrap { ObjectType = objectType; Object = object }: byte array =
        let headerEncoded = 
            sprintf "%s %i%c" (toStr objectType) (object.Length) (Convert.ToChar(0))
            |> Encoding.UTF8.GetBytes
        Array.concat [headerEncoded; object]
    
    let unwrap (content: byte array): Result<GitObject, string> =
        let spaceChar = Convert.ToByte(' ')
        let firstSpace = Array.IndexOf(content, spaceChar)
        let nullChar = Convert.ToChar(0) |> Convert.ToByte
        let firstNull = Array.IndexOf(content, nullChar)
        
        let encodedType = 
            Array.sub content 0 firstSpace
            |> Encoding.UTF8.GetString
            |> fromStr
        let encodedLength = 
            Array.sub content (firstSpace + 1)  (firstNull - (firstSpace + 1))
            |> Encoding.UTF8.GetString
            |> Int32.Parse

        let actualLength = content.Length - (firstNull + 1)

        let object = Array.sub content (firstNull + 1) (actualLength)

        if encodedLength <> actualLength then 
            Error "incorrect git object length"
        else
            encodedType
            |> Result.map (fun t -> { ObjectType = t; Object = object })


type IndexEntryMode = 
    | Mode100644 // normal file
    | Mode100755 // executable
    | Mode120000 // symbolic link
    | Mode040000 // directory

module IndexEntryModes =
    let parse = function
        | "100644" -> Ok Mode100644
        | "100755" -> Ok Mode100755
        | "120000" -> Ok Mode120000
        | "040000" | "40000" -> Ok Mode040000
        | _ -> Error "unsupported mode"

    let toStr = function
        | Mode100644 -> "100644"
        | Mode100755 -> "100755"
        | Mode120000 -> "120000"
        | Mode040000 -> "040000"


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

    let parseTree (content: byte[]) = 
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

    let formatTree { TreeEntries = entries } = 
        let modeToEntryType = function
            | Mode100644 | Mode100755 | Mode120000 -> "blob"
            | Mode040000 -> "tree"
        let formatEntry (TreeEntry(mode, Sha1 hash, path)) =
            let modeStr = mode |> IndexEntryModes.toStr
            let typeStr = mode |> modeToEntryType
            sprintf "%s %s %s    %s\n" modeStr typeStr hash path
        let formatedEntries = entries |> List.map formatEntry
        String.Join("", formatedEntries)