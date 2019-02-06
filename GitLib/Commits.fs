namespace GitLib

type DateWithTimeZone = { DateSeconds: int64; DateTimeZone: string }

type CommitUserData = {
    Name: string
    Email: string
    Date: DateWithTimeZone
}

type Commit = {
    Tree: Sha1
    Parents: Sha1 list
    Author: CommitUserData
    Commiter: CommitUserData
    Message: string
}

module Commits = 
    open System
    open System.Text
    open Utils
    open FParsec

    let private commitParser = 
        let createTime t (tz: char * char[]) =
            let formatTimezone (sign, offset) = new string([| yield sign; yield! offset |])
            { DateSeconds = t; DateTimeZone = (formatTimezone tz) }

        let createUserData (n: char list) (e: char list) date =
            let name = new string(n |> Array.ofList) 
            let email = new string(e |> Array.ofList)
            { 
                Name = name.Trim()
                Email = email.Trim()
                Date = date
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
        let emailParser = pstring "<" >>. (many <| noneOf ['>'; '\n']) .>> pstring ">"
        
        let timeZoneParser = tuple2 (anyOf ['+'; '-']) (parray 4 digit)
        let timeWithTimeZoneParser = 
            pchar ' ' >>. (pipe2 (pint64 .>> pchar ' ') timeZoneParser createTime)

        let userDataParser = (pipe3 nameParser emailParser timeWithTimeZoneParser createUserData) .>> newline
        let authorParser = pstring "author" >>. userDataParser
        let commiterParser = pstring "committer" >>. userDataParser
        let messageParser = (many anyChar) .>> eof

        let commitParser = pipe5 treeParser parentsParser authorParser commiterParser messageParser createCommit
        commitParser

    let parseCommit ({ ObjectType =  _typ; Object = content}): Result<Commit, string> =
        let contentStr = content |> Encoding.UTF8.GetString
        let result = run commitParser contentStr

        match result with
        | Success(result, _state, _position) -> result
        | Failure(errorStr, _error, _state) -> Result.Error errorStr


    let formatCommit ({ 
            Tree = Sha1 tree; Parents = parents; 
            Author = author; Commiter = committer; Message = msg 
        }: Commit) = 

        let formatUserData ({ Name = name; Email = email; Date = date }) = 
            sprintf "%s <%s> %i %s" name email (date.DateSeconds) (date.DateTimeZone)
        
        let lines = 
            seq {
                yield (sprintf "tree %s" tree)
                yield! (parents |> List.map (fun (Sha1 p) -> sprintf "parent %s" p))
                yield (sprintf "author %s" (formatUserData author))
                yield (sprintf "committer %s" (formatUserData committer))
                yield msg
            }
        String.Join("\n", lines) 

    let serializeCommit = 
        formatCommit 
        >> Encoding.UTF8.GetBytes 
        >> (fun bytes -> { ObjectType = Commit; Object = bytes} )
