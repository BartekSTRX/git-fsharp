namespace GitLib

type DateWithTimeZone = { DateSeconds: int64; DateTimeZone: string }

type UserData = {
    Name: string
    Email: string
    Date: DateWithTimeZone
}

module UsersData =
    open FParsec

    let private createTime t (tz: char * char[]) =
        let formatTimezone (sign, offset) = new string([| yield sign; yield! offset |])
        { 
            DateSeconds = t
            DateTimeZone = (formatTimezone tz)
        }

    let private createUserData (n: char list) (e: char list) date =
        let name = new string(n |> Array.ofList) 
        let email = new string(e |> Array.ofList)
        { 
            Name = name.Trim()
            Email = email.Trim()
            Date = date
        }

    let private nameParser = many <| noneOf ['<'; '\n']
    let private emailParser = pstring "<" >>. (many <| noneOf ['>'; '\n']) .>> pstring ">"
        
    let private timeZoneParser = tuple2 (anyOf ['+'; '-']) (parray 4 digit)
    let private timeWithTimeZoneParser = 
        pchar ' ' >>. (pipe2 (pint64 .>> pchar ' ') timeZoneParser createTime)

    let userDataParser: Parser<UserData, unit> = 
        (pipe3 nameParser emailParser timeWithTimeZoneParser createUserData)


    let formatUserData ({ Name = name; Email = email; Date = date }) = 
        sprintf "%s <%s> %i %s" name email (date.DateSeconds) (date.DateTimeZone)
