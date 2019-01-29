module GitLib.Models

type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let private readDecompressed (stream: FileStream) : byte[] =
        use gzipStream = new ZlibStream(stream, CompressionMode.Decompress)
        use memoryStream = new MemoryStream()
        gzipStream.CopyTo(memoryStream)
        memoryStream.ToArray()

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        let content = readDecompressed fileStream
        { Format = format; Content = content }

    let readIndex (rootDir: string) =
        use fileStream = Path.Combine(rootDir, ".git", "index") |> File.OpenRead
        use memoryStream = new MemoryStream()
        fileStream.CopyTo(memoryStream)
        memoryStream.ToArray() |> GitIndexes.parse

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let id1, id2 = Hash.split objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use memoryStream = new MemoryStream(content)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)
