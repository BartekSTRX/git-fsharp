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

module Storage =
    open System.IO

    let private getObjectPath rootDir objectId = 
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        objectPath

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let objectPath = getObjectPath rootDir objectId
        let fileStream = File.OpenRead(objectPath)
        fileStream
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) = 
        let fileStream = readObjectLoose rootDir objectId
        let content = Compression.decompressStream fileStream
        content

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let objectPath = getObjectPath rootDir objectId
        let fileInfo = new FileInfo(objectPath)
        fileInfo.Directory.Create()

        use gzipStream = Compression.compressToStream content
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)

    let deleteObject (rootDir: string) (objectId: Sha1) : unit = 
        let objectPath = getObjectPath rootDir objectId
        File.Delete(objectPath)

        let dirPath = Path.GetDirectoryName(objectPath)
        if Directory.GetFiles(dirPath).Length = 0 then
            Directory.Delete(dirPath)


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

module ReferencesStorage =
    open System.IO
    
    let private getRefPath rootDir ref = 
        Path.Combine(rootDir, ".git", ref)

    let readReference (rootDir: string) (ref: string): string =
        getRefPath rootDir ref |> File.ReadAllText

    let writeReference (rootDir: string) (ref: string) (hash: string): unit=
        let path = getRefPath rootDir ref
        File.WriteAllText(path, hash)

    let deleteReference (rootDir: string) (ref: string): unit = 
        getRefPath rootDir ref |> File.Delete


    let writeTagReference (rootDir: string) (tagName: string) =
        writeReference rootDir (Path.Combine("refs", "tags", tagName))

    let readTagReference (rootDir: string) (tagName: string) =
        readReference rootDir (Path.Combine("refs", "tags", tagName))

    let readAllTagReferences (rootDir: string) =
        Path.Combine(rootDir, ".git", "refs", "tags")
        |> Directory.GetFiles
        |> Array.map Path.GetFileName

    let deleteTagReference (rootDir: string) (tagName: string) =
        deleteReference rootDir (Path.Combine("refs", "tags", tagName))