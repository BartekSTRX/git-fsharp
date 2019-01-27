module Tests

open System
open Xunit
open GitLib.Models

[<Fact>]
let ``Wrap and unwrap object`` () =
    let object = { ObjectType = Blob; Object = "qwertyuiop" }
    let result = object |> GitObjects.wrap |> GitObjects.unwrap
    
    match result with 
    | Ok object2 -> Assert.Equal(object, object2)
    | Error reason -> failwith reason
    
[<Fact>]
let ``Unwrap and wrap object`` () =
    let content = "reee"
    let wrappedObject = sprintf "%s %i%c%s" "blob" 4 (Convert.ToChar(0)) content

    let result = wrappedObject |> GitObjects.unwrap |> Result.map GitObjects.wrap

    match result with 
    | Ok object -> Assert.Equal(wrappedObject, object)
    | Error reason -> failwith reason
