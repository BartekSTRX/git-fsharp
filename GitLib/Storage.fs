namespace GitLib

module Compression = 
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let decompressStream (stream: Stream) : byte[] =
        use gzipStream = new ZlibStream(stream, CompressionMode.Decompress)
        use resultStream = new MemoryStream()
        gzipStream.CopyTo(resultStream)
        resultStream.ToArray()

    let decompressArray (bytes: byte[]) : byte[] =
        use sourceStream = new MemoryStream(bytes)
        decompressStream sourceStream

    let compressToStream (bytes: byte[]) : Stream =
        let memoryStream = new MemoryStream(bytes)
        let gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)
        gzipStream :> Stream
    

type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO

    let private getObjectPath rootDir objectId = 
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        objectPath

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let objectPath = getObjectPath rootDir objectId
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        let content = Compression.decompressStream fileStream
        { Format = format; Content = content }

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let objectPath = getObjectPath rootDir objectId
        let fileInfo = new FileInfo(objectPath)

        fileInfo.Directory.Create()

        use gzipStream = Compression.compressToStream content
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)


    let private getIndexPath rootDir = Path.Combine(rootDir, ".git", "index")

    let indexExists = getIndexPath >> File.Exists
    
    let readIndex (rootDir: string) : byte[] =
        use fileStream = rootDir |> getIndexPath |> File.OpenRead
        use memoryStream = new MemoryStream()
        fileStream.CopyTo(memoryStream)
        memoryStream.ToArray()

    let writeIndex (rootDir: string) (indexBytes: byte[]) = 
        use fileStream = rootDir |> getIndexPath |> File.OpenWrite
        fileStream.Write(indexBytes, 0, indexBytes.Length)

module References =
    open System.IO
    
    let private getRefPath rootDir ref = 
        Path.Combine(rootDir, ".git", ref)

    let readReference (rootDir: string) (ref: string): string =
        getRefPath rootDir ref |> File.ReadAllText

    let writeReference (rootDir: string) (ref: string) (value: string): unit=
        let path = getRefPath rootDir ref
        File.WriteAllText(path, value)

    let deleteReference (rootDir: string) (ref: string): unit = 
        getRefPath rootDir ref |> File.Delete