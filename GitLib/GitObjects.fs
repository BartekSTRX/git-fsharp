namespace GitLib

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

