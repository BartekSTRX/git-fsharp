namespace GitLib

type Commit = {
    Tree: Sha1
    Parents: Sha1 list
    Author: UserData
    Commiter: UserData
    Message: string
}

module Commits = 
    open System
    open System.Text
    open Utils
    open FParsec

    let private commitParser = 
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

        let authorParser = pstring "author" >>. UsersData.userDataParser .>> newline
        let commiterParser = pstring "committer" >>. UsersData.userDataParser .>> newline
        let messageParser = (many anyChar) .>> eof

        let commitParser = pipe5 treeParser parentsParser authorParser commiterParser messageParser createCommit
        commitParser

    let parseCommit ({ ObjectType =  _type; Object = content}): Result<Commit, string> =
        let contentStr = content |> Encoding.UTF8.GetString
        let result = run commitParser contentStr

        match result with
        | Success(result, _state, _position) -> result
        | Failure(errorStr, _error, _state) -> Result.Error errorStr


    let formatCommit ({ 
            Tree = Sha1 tree; Parents = parents; 
            Author = author; Commiter = committer; Message = msg 
        }: Commit) = 
        
        let lines = 
            seq {
                yield (sprintf "tree %s" tree)
                yield! (parents |> List.map (fun (Sha1 p) -> sprintf "parent %s" p))
                yield (sprintf "author %s" (UsersData.formatUserData author))
                yield (sprintf "committer %s" (UsersData.formatUserData committer))
                yield msg
            }
        String.Join("\n", lines)

    let serializeCommit = 
        formatCommit 
        >> Encoding.UTF8.GetBytes 
        >> (fun bytes -> { ObjectType = Commit; Object = bytes} )
