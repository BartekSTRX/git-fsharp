namespace GitLib

type Blob = { Content:byte[] }

module Blobs =
    open System.Text
    
    let parseBlob ({ ObjectType = _typ; Object = content }) : Result<Blob, string> =
        { Content = content }|> Ok

    let serializeBlob ({ Content = content }: Blob) : GitObject =
        { ObjectType = Blob; Object = content }

    let formatBlob ({ Content = content }): string =
        content |> Encoding.UTF8.GetString

