namespace GitLib

type Tag = {
    ObjectId: Sha1
    TaggedObjectType: ObjectType
    Name: string
    TaggerData: UserData
    TagMessage: string
}

module Tags =
    open System.Text
    open FParsec
    open Utils
    open System

    let private tagParser = 
        let createTag objectHash objectType tagName tagger message = 
            let charListToString cl = new string(cl |> Array.ofList)
            result {
                let! hash = new string(objectHash:char[]) |> Hash.parse
                let! oType = objectType |> charListToString |> ObjectTypes.fromStr 

                return {
                    ObjectId = hash
                    TaggedObjectType = oType
                    Name = (tagName |> charListToString)
                    TaggerData = tagger
                    TagMessage = (message|> charListToString)
                }
            }

        let sha1Parser = parray 40 hex
        let objectParser = pstring "object" >>. pchar ' ' >>. sha1Parser .>> newline
        let typeParser = pstring "type" >>. pchar ' ' >>. (many <| noneOf ['\n']) .>> newline
        let tagNameParser = pstring "tag" >>. pchar ' ' >>. (many <| noneOf ['\n']) .>> newline
        let taggerParser = pstring "tagger" >>. UsersData.userDataParser .>> newline
        let messageParser = (many anyChar) .>> eof

        let tagParser = pipe5 objectParser typeParser tagNameParser taggerParser messageParser createTag
        tagParser

    let parseTag ({ ObjectType = _type; Object = content }) : Result<Tag, string> = 
        let contentStr = content |> Encoding.UTF8.GetString
        let result = run tagParser contentStr

        match result with
        | Success(result, _state, _position) -> result
        | Failure(errorStr, _error, _state) -> Result.Error errorStr

    let formatTag ({
            ObjectId = Sha1 hash; TaggedObjectType = oType
            Name = name; TaggerData = tagger; TagMessage = message
        }: Tag) = 

        let lines = 
            seq {
                yield (sprintf "object %s" hash)
                yield (sprintf "type %s" (ObjectTypes.toStr oType))
                yield (sprintf "tag %s" name)
                yield (sprintf "tagger %s" (UsersData.formatUserData tagger))
                yield message
            }
        String.Join("\n", lines)

    let serializeTag = 
        formatTag
        >> Encoding.UTF8.GetBytes 
        >> (fun bytes -> { ObjectType = Tag; Object = bytes} )
