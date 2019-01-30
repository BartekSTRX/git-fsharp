namespace GitLib

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


    let getIndexPath rootDir = Path.Combine(rootDir, ".git", "index")

    let readIndex (indexPath: string) =
        use fileStream = indexPath |> File.OpenRead
        use memoryStream = new MemoryStream()
        fileStream.CopyTo(memoryStream)
        memoryStream.ToArray() |> GitIndexes.parse

    let writeIndex (rootDir: string) (index: GitIndex) = 
        use fileStream = rootDir |> getIndexPath |> File.OpenWrite
        let bytes = index |> GitIndexes.serialize
        fileStream.Write(bytes, 0, bytes.Length)

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let id1, id2 = Hash.split objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use memoryStream = new MemoryStream(content)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)
