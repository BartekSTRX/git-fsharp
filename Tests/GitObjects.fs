module Tests

open System
open Xunit
open GitLib.Models
open System.IO
open GitLib
open System.Text

[<Fact>]
let ``Wrap and unwrap object`` () =
    let object = { 
        ObjectType = Blob
        Object = Encoding.UTF8.GetBytes("qwertyuiop") 
    }
    let result = object |> GitObjects.wrap |> GitObjects.unwrap
    
    match result with 
    | Ok object2 -> Assert.Equal(object, object2)
    | Error reason -> failwith reason
    
[<Fact>]
let ``Unwrap and wrap object`` () =
    let content = "reee"
    let wrappedObject = 
        sprintf "%s %i%c%s" "blob" 4 (Convert.ToChar(0)) content
        |> Encoding.UTF8.GetBytes

    let result = wrappedObject |> GitObjects.unwrap |> Result.map GitObjects.wrap

    match result with 
    | Ok object -> Assert.Equal<byte[]>(wrappedObject, object)
    | Error reason -> failwith reason

[<Fact>]
let ``Hash blob object`` () =
    let rootDir = Directory.GetCurrentDirectory()
    let path = Path.Combine("TestData", "SampleTextFile.txt")

    let (Sha1 hash) = Commands.hashObject rootDir rootDir path false
    Assert.Equal("26896bbf9c17fd5475973450ec83f7d8d84575bb", hash)