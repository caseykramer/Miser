module Miser.``Parser Tests``
    open NUnit.Framework
    open FsUnit
    open Parser

    module ``Comments`` =

        [<Test>]
        let ``Can parse single in-line comment starting with // from single line`` () = 
            let testData = "// This is a single in-line comment with no additional lines"
            match [testData] with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment with no additional lines")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment starting with # from single line`` () = 
            let testData = "# This is a signle in-line comment starting with #"
            match [testData] with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a signle in-line comment starting with #")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment with multiple lines`` () = 
            let testData = ["// This is a single in-line comment"
                            "This is something else"]
            match testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment");rest |> List.head |> should equal "This is something else"
            | _ -> Assert.Fail()

        [<Test>]
        let ``Can parse a multi-line block comment`` () = 
            let testData = ["/* This is a "
                            "multi-line block style "
                            "comment */"]
            match testData with
            | BlockComment (comment,rest) ->
                match comment with
                | Ast.CommentBlock (comment) -> comment |> should equal ([" This is a ";"multi-line block style "; "comment "])
                | _ -> Assert.Fail()
            | _ -> Assert.Fail()
    module Identitiers = 
        
        [<Test>]
        let ``Can parse a standard alpha-numeric identifier`` () =
            let testData = "someIdentifier"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "someIdentifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Can parse identifier starting with an underscore`` () =
            let testData = "_someIdentifier"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "_someIdentifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain dashes`` () =
            let testData = "some-identifier"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some-identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain dots`` () =
            let testData = "some.identifier"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some.identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain underscores`` () =
            let testData = "some_identifier"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some_identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain letters, numbers, dashes, underscores, and dots`` () = 
            let testData = "_some-big-Identifier.WithALittle-Bit.Of.Everything42"
            match testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "_some-big-Identifier.WithALittle-Bit.Of.Everything42")
            | _ -> Assert.Fail()