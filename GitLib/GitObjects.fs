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


type CommitUserData = {
    Name: string
    Email: string
    DateSeconds: int64 option
    DateTimeZone: string option
}

type Commit = {
    Tree: Sha1
    Parents: Sha1 list
    Author: CommitUserData
    Commiter: CommitUserData
    Message: string
}

module Commits = 
    open System.Text
    open Utils
    open FParsec

    let private commitParser = 
        let createUserData (n: char list) (e: char list) t (tz: option<char * char[]>) =
            let formatTimezone = Option.map (fun ((sign, offset)) -> new string([| yield sign; yield! offset |]))
            { 
                Name = new string(n |> Array.ofList); 
                Email = new string(e |> Array.ofList); 
                DateSeconds = t; 
                DateTimeZone = formatTimezone tz
            }
        let createCommit treeHash parents author committer message = 
            result {
                let! hash = new string(treeHash:char[]) |> Hash.parse
                let! parentsHashes = 
                        parents 
                        |> List.map (fun (x: char[]) -> new string(x) |> Hash.parse) 
                        |> traverse
                return {
                    Tree = hash
                    Parents = parentsHashes
                    Author = author
                    Commiter = committer
                    Message = new string(message |> Array.ofList)
                }
            }

        let sha1Parser = parray 40 hex
        let treeParser = pstring "tree" >>. pchar ' ' >>. sha1Parser .>> newline
        let parentParser = pstring "parent" >>. pchar ' ' >>. sha1Parser .>> newline
        let parentsParser = many parentParser
        let nameParser = many <| noneOf ['<'; '\n']
        let emailParser = pstring "<" >>. (many <| noneOf ['>'; '\n']) .>> pstring ">" .>> pchar ' '
        let timeParser = pint64 .>> pchar ' ' |> opt
        let timezoneParser = tuple2 (anyOf ['+'; '-']) (parray 4 digit) .>> newline |> opt
        let userDataParser = pipe4 nameParser emailParser timeParser timezoneParser createUserData
        let authorParser = pstring "author" >>. userDataParser
        let commiterParser = pstring "committer" >>. userDataParser
        let messageParser = (many anyChar) .>> eof

        let commitParser = pipe5 treeParser parentsParser authorParser commiterParser messageParser createCommit
        commitParser

    let parseCommit (content: byte[]): Result<Commit, string> =
        let contentStr = content |> Encoding.UTF8.GetString
        let result = run commitParser contentStr

        match result with
        | Success(result, _state, _position) -> result
        | Failure(errorStr, _error, _state) -> Result.Error errorStr

    let formatCommit commit = 
        ""
