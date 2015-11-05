module ``Apache Test Definitions``

open System.IO
open System.Net
open System.Net.Http
open System.Text
open FParsec 
open NUnit.Framework
open FsUnit

let private getDef (url:string) =
    async {
        let req:HttpWebRequest = HttpWebRequest.CreateHttp url
        let! resp = req.AsyncGetResponse()
        let s = resp.GetResponseStream()
        use r = new StreamReader(s)
        return r.ReadToEnd() }

let [<Literal>] ThriftTest = "https://git-wip-us.apache.org/repos/asf?p=thrift.git;a=blob_plain;f=test/ThriftTest.thrift;hb=HEAD"

[<Test>]
let ``Can parse the Apache Project "ThriftTest.thrift" thrift definition``() = 
    let thriftTest = getDef ThriftTest |> Async.RunSynchronously
    let result = run ThriftParser.document thriftTest
    match result with
    | Success(r,_,_) -> 
        match r with
        | ThriftAST.Document(headers,definitions) ->
            headers |> List.length |> should equal 18
            definitions |> List.length |> should equal 31
    | Failure(err,_,_) -> Assert.Fail(err)

let [<Literal>] FBThriftTest = "https://git-wip-us.apache.org/repos/asf?p=thrift.git;a=blob_plain;f=contrib/fb303/if/fb303.thrift;hb=HEAD"

[<Test>]
let ``Can parse the Apache Project "FB303.thrift" definition``() = 
    let thriftTest = getDef FBThriftTest |> Async.RunSynchronously
    let result = run ThriftParser.document thriftTest
    match result with
    | Success(r,_,_) ->
        match r with
        | ThriftAST.Document(headers,definitions) ->
            headers |> List.length |> should be (greaterThan 0)
            definitions |> List.length |> should be (greaterThan 0)
    | Failure(err,_,_) -> Assert.Fail(err)

let [<Literal>] CassandraThrift = "http://svn.apache.org/viewvc/cassandra/trunk/interface/cassandra.thrift?view=co"

[<Test>]
let ``Can parse the Cassandra Project "cassandra.thrift" definition``() = 
    let thriftTest = getDef CassandraThrift |> Async.RunSynchronously
    let result = run ThriftParser.document thriftTest
    match result with
    | Success(r,_,_) ->
        match r with
        | ThriftAST.Document(headers,definitions) ->
            headers |> List.length |> should be (greaterThan 0)
            definitions |> List.length |> should be (greaterThan 0)
    | Failure(err,_,_) -> Assert.Fail(err)