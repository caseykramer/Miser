module ``Parser Tests``
   
open FParsec
open NUnit.Framework
open FsUnit

module Header = 
    
    module Include = 
        
        [<Test>]
        let ``Can parse a basic include``() = 
            let includeStr = """include "somefile.thrift" """
            let result = run ThriftParser.includeDef includeStr
            match result with
            | Success (ThriftAST.Include (ThriftAST.StringLiteral s),_,_) -> s |> should equal "somefile.thrift"
            | Failure (err,_,_) -> Assert.Fail(err)
