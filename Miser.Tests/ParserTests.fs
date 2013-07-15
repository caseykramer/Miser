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
    module ``Literals`` = 
        [<Test>]
        let ``StringLiteral can be delimited by double-quotes`` () =
            let testData = "\"Some Literal\""
            match testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "Some Literal")
            | _ -> Assert.Fail()    

        [<Test>]
        let ``StringLiteral can be delimited by sing-quotes`` () = 
            let testData = "'Some Literal'"
            match testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "Some Literal")
            | _ -> Assert.Fail()

        [<Test>]
        let ``StringLiteral delimited by double quotes may contain single quotes`` () =
            let testData = "\"This test musn't fail\""
            match testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "This test musn't fail")
            | _ -> Assert.Fail()

        [<Test>]
        let ``StringLiteral delimited by single quotes may countain double quotes`` () =
            let testData = "'This is \"The real deal\"'"
            match testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "This is \"The real deal\"")
            | _ -> Assert.Fail()

    module ``Includes`` =
        [<Test>]
        let ``Include is indicated by the use of the keyword "import"`` () =
            let testData = "import \"Something.thrift\""
            match testData with 
            | ThriftInclude (inc,_) -> inc |> should equal (Ast.Include(Ast.StringLiteral "Something.thrift"))
            | _ -> Assert.Fail()
    
    module ``Namespace`` =
        
        [<Test>]
        let ``Namespaces can be defined with global scope using *`` () = 
            let testData = "namespace * com.test.testing"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Any,Ast.Identifier "com.test.testing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for C++ scope using 'cpp'`` () =
            let testData = "namespace cpp testing_this_thing"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Cpp,Ast.Identifier "testing_this_thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Java scope using 'java'`` () =
            let testData = "namespace java com.testing.this.thing"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Java,Ast.Identifier "com.testing.this.thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Python scope using 'py'`` () =
            let testData = "namespace py some_py"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Py,Ast.Identifier "some_py"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Perl scope using 'perl'`` () = 
            let testData = "namespace perl why_even_try"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Perl,Ast.Identifier "why_even_try"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Ruby scope using 'rb'`` () =
            let testData = "namespace rb ruby.thing"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Rb,Ast.Identifier "ruby.thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Cocoa scope using 'cocoa'`` () =
            let testData = "namespace cocoa im_on_a_mac"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Cocoa,Ast.Identifier "im_on_a_mac"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for CSharp scope using 'csharp'`` () =
            let testData = "namespace csharp MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()