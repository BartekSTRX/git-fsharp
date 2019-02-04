namespace GitLib


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
            let name = new string(n |> Array.ofList) 
            let email = new string(e |> Array.ofList)
            let formatTimezone = Option.map (fun ((sign, offset)) -> new string([| yield sign; yield! offset |]))
            { 
                Name = name.Trim()
                Email = email.Trim(); 
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
