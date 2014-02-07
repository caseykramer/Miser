module Miser.``Parser Tests``
    open NUnit.Framework
    open FsUnit
    open Parser
    
    module ``Comments`` =

        [<Test>]
        let ``Can parse single in-line comment starting with // from single line`` () = 
            let testData = "// This is a single in-line comment with no additional lines"
            match asInput testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment with no additional lines")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment starting with # from single line`` () = 
            let testData = "# This is a single in-line comment starting with #"
            match asInput testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment starting with #")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment with multiple lines`` () = 
            let testData = """// This is a single in-line comment
This is something else"""
            match asInput testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment");rest |> fst |> toString |> should equal "\r\nThis is something else"
            | _ -> Assert.Fail()

        [<Test>]
        let ``Can parse a multi-line block comment`` () = 
            let testData = """/* This is a
multi-line block style
comment */"""
            match asInput testData with
            | BlockComment (comment,rest) ->
                match comment with
                | Ast.CommentBlock (comment) -> comment |> should equal (" This is a\r\nmulti-line block style\r\ncomment ")
                | _ -> Assert.Fail()
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse doc-comments`` () = 
            let testdata = """/**
 * This is a sample doc comment
 */"""
            match asInput testdata with
            | DocComment (comment,_) -> comment |> should equal (Ast.DocComment("\r\n * This is a sample doc comment\r\n "))
            | _ -> Assert.Fail()
    module ``Identifiers`` =
        [<Test>]
        let ``Can parse a standard alpha-numeric identifier`` () =
            let testData = "someIdentifier"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "someIdentifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Can parse identifier starting with an underscore`` () =
            let testData = "_someIdentifier"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "_someIdentifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain dashes`` () =
            let testData = "some-identifier"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some-identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain dots`` () =
            let testData = "some.identifier"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some.identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain underscores`` () =
            let testData = "some_identifier"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "some_identifier")
            | _ -> Assert.Fail()

        [<Test>]
        let ``Identifiers can contain letters, numbers, dashes, underscores, and dots`` () = 
            let testData = "_some-big-Identifier.WithALittle-Bit.Of.Everything42"
            match asInput testData with
            | Identifier (ident,_) -> ident |> should equal (Ast.Identifier "_some-big-Identifier.WithALittle-Bit.Of.Everything42")
            | _ -> Assert.Fail()
    module ``Literals`` = 
        [<Test>]
        let ``StringLiteral can be delimited by double-quotes`` () =
            let testData = "\"Some Literal\""
            match asInput testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "Some Literal")
            | _ -> Assert.Fail()    

        [<Test>]
        let ``StringLiteral can be delimited by sing-quotes`` () = 
            let testData = "'Some Literal'"
            match asInput testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "Some Literal")
            | _ -> Assert.Fail()

        [<Test>]
        let ``StringLiteral delimited by double quotes may contain single quotes`` () =
            let testData = "\"This test mustn't fail\""
            match asInput testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "This test mustn't fail")
            | _ -> Assert.Fail()

        [<Test>]
        let ``StringLiteral delimited by single quotes may countain double quotes`` () =
            let testData = "'This is \"The real deal\"'"
            match asInput testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "This is \"The real deal\"")
            | _ -> Assert.Fail()
    module ``Includes`` =
        [<Test>]
        let ``Include is indicated by the use of the keyword "import"`` () =
            let testData = "import \"Something.thrift\""
            match asInput testData with 
            | ThriftInclude (inc,_) -> inc |> should equal (Ast.Include(Ast.StringLiteral "Something.thrift"))
            | _ -> Assert.Fail()
    module ``Namespace`` =        
        [<Test>]
        let ``Namespaces can be defined with global scope using *`` () = 
            let testData = "namespace * com.test.testing"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Any,Ast.Identifier "com.test.testing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for C++ scope using 'cpp'`` () =
            let testData = "namespace cpp testing_this_thing"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Cpp,Ast.Identifier "testing_this_thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Java scope using 'java'`` () =
            let testData = "namespace java com.testing.this.thing"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Java,Ast.Identifier "com.testing.this.thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Python scope using 'py'`` () =
            let testData = "namespace py some_py"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Py,Ast.Identifier "some_py"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Perl scope using 'perl'`` () = 
            let testData = "namespace perl why_even_try"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Perl,Ast.Identifier "why_even_try"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Ruby scope using 'rb'`` () =
            let testData = "namespace rb ruby.thing"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Rb,Ast.Identifier "ruby.thing"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for Cocoa scope using 'cocoa'`` () =
            let testData = "namespace cocoa im_on_a_mac"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Cocoa,Ast.Identifier "im_on_a_mac"))
            | _ -> Assert.Fail()
        [<Test>]
        let ``Namespace can be defined for CSharp scope using 'csharp'`` () =
            let testData = "namespace csharp MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // These were in the sample, but not the IDL definition
        // c_glib
        [<Test>]
        let ``Namespace can be defined for C_Glib scope using 'c_glib'`` () =
            let testData = "namespace c_glib MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.C_Glib,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // py.twisted
        [<Test>]
        let ``Namespace can be defined for Py_Twisted scope using 'py.twisted'`` () =
            let testData = "namespace py.twisted MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Py_Twisted,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // go
        [<Test>]
        let ``Namespace can be defined for Go scope using 'go'`` () =
            let testData = "namespace go MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Go,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // delphi
        [<Test>]
        let ``Namespace can be defined for Delphi scope using 'delphi'`` () =
            let testData = "namespace delphi MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Delphi,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // js
        [<Test>]
        let ``Namespace can be defined for Javascript scope using 'js'`` () =
            let testData = "namespace js MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Javascript,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // st
        [<Test>]
        let ``Namespace can be defined for Smalltalk scope using 'st'`` () =
            let testData = "namespace st MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Smalltalk,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // Others should compile with warnings
        [<Test>]
        let ``Namespace can be defined for Other scope using a non-registered designator`` () =
            let testData = "namespace someother MyNamespace.Net"
            match asInput testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Other("someother"),Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
    module ``Header`` =         
        [<Test>]
        let ``Header can contain just an include`` () = 
            let testData = """import "somefile.thrift" """
            match asInput testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "somefile.thrift")))]
            | _ -> Assert.Fail()
        
        [<Test>]
        let ``Header can contain multiple includes`` () = 
            let testData = """import "somefile.thrift" 
                              import "someotherfile.thrift" """
            match asInput testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "somefile.thrift")));
                                                               (Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "someotherfile.thrift")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain just a namespace`` () = 
            let testData = "namespace csharp SomeNamespace.Net"
            match asInput testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "SomeNamespace.Net")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain multiple Namespaces`` () = 
            let testData = """namespace csharp SomeNamespace.Net
                              namespace java org.java.something"""
            match asInput testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "SomeNamespace.Net")))
                                                                 (Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.Java,Ast.Identifier "org.java.something")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain includes and namespaces`` () = 
            let testData = """import "somefile.thrift"
                              import "anotherfile.thrift"

                              namespace csharp SomeNamespace.Net
                              namespace java org.java.something"""

            match asInput testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "somefile.thrift")))
                                                                 (Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "anotherfile.thrift")))
                                                                 
                                                                 (Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "SomeNamespace.Net")))
                                                                 (Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.Java,Ast.Identifier "org.java.something")))]
            | _ -> Assert.Fail()
    module ``Types`` =
        module ``Base Types`` = 
            [<Test>]
            let ``Can parse a bool base type`` () =
                let testdata = "bool"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Bool
                | _ -> Assert.Fail()
            
            [<Test>]
            let ``Can parse a byte base type`` () = 
                let testdata = "byte"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Byte
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int16 base type`` () = 
                let testdata = "i16"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I16
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int32 base type`` () = 
                let testdata = "i32"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I32
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int32 base type using 'int'`` () = 
                let testdata = "int"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I32
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int64 base type`` () = 
                let testdata = "i64"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I64
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a double base type`` () = 
                let testdata = "double"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Double
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a string base type`` () = 
                let testdata = "string"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.String
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a binary base type`` () = 
                let testdata = "binary"
                match asInput testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Binary
                | _ -> Assert.Fail()
        module ``Container Types`` = 
            [<Test>]
            let ``Can parse a list type`` () =
                let testdata = "list< i32 >"
                match asInput testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.List(Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting List(BaseField(I32))"
            [<Test>]
            let ``Can parse a set type`` () =
                let testdata = "set<i32>"
                match asInput testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.Set(Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting Set(BaseField(I32))"
            [<Test>]
            let ``Can parse a map type`` () =
                let testdata = "map<string, i32>"
                match asInput testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.Map(Ast.FieldType.BaseField(Ast.BaseType.String),Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting Map(BaseField(String),BaseField(I32))"
            [<Test>]
            let ``Can parse nested container type`` () = 
                let testdata = "list<map<i32,set<map<string,list<i32>>>>>"
                match asInput testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.List(
                                                                        Ast.ContainerField(
                                                                            Ast.Map(
                                                                                Ast.BaseField(Ast.I32),
                                                                                Ast.ContainerField(
                                                                                    Ast.Set(
                                                                                        Ast.ContainerField(
                                                                                            Ast.Map(
                                                                                                Ast.BaseField(Ast.String),
                                                                                                Ast.ContainerField(
                                                                                                    Ast.List(
                                                                                                        Ast.BaseField(Ast.I32)
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                    ))
                | _ -> failwith "Expecting List(ContainerField(Map(BaseField(I32),ContainerField(Set(ContainerField(Map(BaseField(String,List(BaseField(I32))))))))))"
        module ``Definition Types`` = 
            [<Test>]
            let ``Can parse a definition type based on a BaseType`` () = 
                match asInput "i32" with
                | DefinitionType (dtype,_) -> dtype |> should equal (Ast.DefinitionType.BaseDefinition(Ast.BaseType.I32))
                | _ -> failwith "Expecting DefinitionType(BaseField(String),BaseField(I32))"
            [<Test>]
            let ``Can parse a definition type based on a ContainerType`` () = 
                let testdata = "list<i32>"
                match asInput testdata with
                | DefinitionType(dtype,_) -> dtype |> should equal (Ast.DefinitionType.ContainerDefinition(Ast.ContainerType.List(Ast.FieldType.BaseField(Ast.BaseType.I32))))
                | _ -> failwith "Expecting DefinitionType(ContainerDefinition(Set(BaseField(I32))))"
        module ``Field Types`` = 
            [<Test>]
            let ``Can parse an Identifier as a FieldType`` () = 
                let testdata = "MyIdentifier"
                match asInput testdata with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.IdentifierField(Ast.Identifier("MyIdentifier")))
                | _ -> failwith "Expecting FieldType(Identifier(\"MyIdentifier\"))"
            [<Test>]
            let ``Can parse a BaseType as a FieldType`` () = 
                match asInput "string" with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.BaseField(Ast.BaseType.String))
                | _ -> failwith "Expecting FieldType(BaseType.String)"
            [<Test>]
            let ``Can parse a ContainerType as a FieldType`` () = 
                let testdata = "map<string,string>"
                match asInput testdata with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.ContainerField(Ast.ContainerType.Map(Ast.FieldType.BaseField(Ast.BaseType.String),Ast.FieldType.BaseField(Ast.BaseType.String))))
                | _ -> failwith "Expecting FieldType(ContainerType.Map(FieldType(BaseType.String),FieldType(BaseType.String)))"
    module ``Definitions`` =
        module ``Constants`` = 
            [<Test>]
            let ``Can parse Constant with string literal value`` () = 
                let testdata = "const string MY_STRING = \"mystringliteral\""
                match asInput testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.String),Ast.Identifier("MY_STRING"),Ast.ConstantValue.LiteralConstant(Ast.StringLiteral("mystringliteral")))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseTpe.String),Identifier(\"MY_STRING\"),ConstantValue.LiteralConstant(\"mystringliteral\"))"
            [<Test>]
            let ``Can parse Constant with integer literal value`` () = 
                let testdata = "const i32 MY_INT32 = 12345"
                match asInput testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.I32),Ast.Identifier("MY_INT32"),Ast.ConstantValue.IntConstant(12345))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.I32),Identifier(\"MY_INT32\"),ConstantValue.IntConstant(12345))"
            [<Test>]
            let ``Can parse Constant with negative integer literal value`` () = 
                let testdata = "const i32 MY_NEG_INT32 = -12345"
                match asInput testdata with
                | Constant(constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.I32),Ast.Identifier("MY_NEG_INT32"),Ast.ConstantValue.IntConstant(-12345))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.I32),Identifier(\"MY_NEG_INT32\"),ConstantValue.IntConstant(-12345))"
            [<Test>]
            let ``Can parse Constant with double literal value`` () = 
                let testdata = "const double MY_DOUBLE = 123.45"
                match asInput testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.Double),Ast.Identifier("MY_DOUBLE"),Ast.ConstantValue.DoubleConstant(123.45))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.Double),Identifier(\"MY_DOUBLE\"),ConstantValue.DoubleConstant(123.45))"
            [<Test>]
            let ``Can parse Constant with literal List value`` () =
                let testdata = "const list<i32> MY_LIST = [1,2,3,4,5]"
                match asInput testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.ContainerField(Ast.List(Ast.BaseField(Ast.I32))),
                                                                                                   Ast.Identifier("MY_LIST"),
                                                                                                   Ast.ConstantValue.ListConstant([Ast.ConstantValue.IntConstant(1);
                                                                                                                                   Ast.ConstantValue.IntConstant(2);
                                                                                                                                   Ast.ConstantValue.IntConstant(3);
                                                                                                                                   Ast.ConstantValue.IntConstant(4);
                                                                                                                                   Ast.ConstantValue.IntConstant(5)]))))
                | _ -> failwith "Expecting ConstDefinition(Const(List(BaseField(I32)),Identifier(\"MY_LIST\"),ListConstant([IntConstant(1);IntConstant(2);IntConstant(3);IntConstant(4);IntConstant(5)))"
            [<Test>]
            let ``Can parse Constant with literal Map value`` () = 
                let testdata = "const map<string,i32> MY_MAP = {\"one\":1;\"two\":2;\"three\":3}"
                match asInput testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.ContainerField(Ast.Map(Ast.BaseField(Ast.String),Ast.BaseField(Ast.I32))),
                                                                                                   Ast.Identifier("MY_MAP"),
                                                                                                   Ast.ConstantValue.MapConstant([(Ast.ConstantValue.LiteralConstant(Ast.StringLiteral("one")),Ast.ConstantValue.IntConstant(1))
                                                                                                                                  (Ast.ConstantValue.LiteralConstant(Ast.StringLiteral("two")),Ast.ConstantValue.IntConstant(2))
                                                                                                                                  (Ast.ConstantValue.LiteralConstant(Ast.StringLiteral("three")),Ast.ConstantValue.IntConstant(3))
                                                                                                                                 ] |> Map.ofList))))
                | _ -> failwith "Expecting ConstDefinition(Const(Map(BaseField(String),BaseField(I32)),Identifier(\"MY_MAP\"),MapConstant({ LiteralConstant(StringLiteral(\"one\"):IntConstant(1);LiteralConstant(StringLiteral(\"twp\"):IntConstant(2);LiteralConstant(StringLiteral(\"three\"):IntConstant(3)}))"
        module ``typedef`` =             
            [<Test>]
            let ``Can parse a type definition`` () =
                let testData = "typedef i32 MyInteger"
                match asInput testData with
                | TypeDef (def,_) -> def |> should equal (Ast.TypeDef(Ast.BaseDefinition(Ast.BaseType.I32),Ast.Identifier("MyInteger")))
                | _ -> Assert.Fail()
        module ``enum`` = 
            [<Test>]
            let ``Can parse an enum without integer value assignments`` () = 
                let testdata = """enum MyEnum {
                    FIRST,
                    SECOND,
                    THIRD }"""
                match asInput testdata with
                | Enum (enum,_) -> enum |> should equal (Ast.EnumDefinition(Ast.Enum(Ast.Identifier("MyEnum"),[(0,Ast.Identifier("FIRST"));(1,Ast.Identifier("SECOND"));(2,Ast.Identifier("THIRD"))])))
                | _ -> failwith "Expecting EnumDefinition(Enum(Identifier(\"MyEnum\"),[(0,Identifier(\"FIRST\"));(1,Identifier(\"SECOND\"));(2,Identifier(\"THIRD\"))]))"
            [<Test>]
            let ``Can parse an enum with integer value assignments`` () = 
                let testdata = """enum MyEnum {
                    FIRST = 1,
                    SECOND = 2,
                    THIRD = 4 }"""
                match asInput testdata with
                | Enum (enum,_) -> enum |> should equal (Ast.EnumDefinition(Ast.Enum(Ast.Identifier("MyEnum"),[(1,Ast.Identifier("FIRST"));(2,Ast.Identifier("SECOND"));(4,Ast.Identifier("THIRD"))])))
                | _ -> failwith "Expecting EnumDefinition(Enum(Identifier(\"MyEnum\"),[(1,Identifier(\"FIRST\"));(2,Identifier(\"SECOND\"));(4,Identifier(\"THIRD\"))]))"
        module ``struct`` = 
            [<Test>]
            let ``Can parse a Struct with numbered fields, no defaults or optionals`` () = 
                let testdata = """struct Work {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: string comment }"""
                match asInput testdata with
                | Struct (strct,_) -> 
                    printf "%A" strct
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None))
                                                                           Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                           Ast.NumberedField(3,Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                           Ast.NumberedField(4,Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[NumberedField(1,Field(BaseField(BaseType.I32),Identifier(\"num1\"),None)),NumberedField(2,Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),NumberedField(3,Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),NumberedField(4,Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
            [<Test>]
            let ``Can parse a Struct with numbered fields, no defaults, and optional fields`` () = 
                let testdata = """struct Work {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: optional string comment }"""
                match asInput testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None))
                                                                           Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                           Ast.NumberedField(3,Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                           Ast.NumberedField(4,Ast.OptionalField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[NumberedField(1,Field(BaseField(BaseType.I32),Identifier(\"num1\"),None)),NumberedField(2,Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),NumberedField(3,Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),NumberedField(4,Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
            [<Test>]
            let ``Can parse a Struct with numbered fields, no defaults, and required fields`` () = 
                let testdata = """struct Work {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: required string comment }"""
                match asInput testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None))
                                                                           Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                           Ast.NumberedField(3,Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                           Ast.NumberedField(4,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[NumberedField(1,Field(BaseField(BaseType.I32),Identifier(\"num1\"),None)),NumberedField(2,Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),NumberedField(3,Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),NumberedField(4,RequiredField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None)))])))"

            [<Test>]
            let ``Can parse a Struct with unnumbered fields, no defaults, and no optional fields`` () = 
                let testdata = """struct Work {
                    i32 num,
                    i32 num2,
                    Operation op,
                    string comment }"""
                match asInput testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None)
                                                                           Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)
                                                                           Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None)
                                                                           Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[Field(BaseField(BaseType.I32),Identifier(\"num1\"),None),Field(BaseField(BaseType.I32),Identifier(\"num2\"),None),Field(IdentifierField(Identifier(\"Operation\"),Identifier(\"op\"),None)),OptionalField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
            [<Test>]
            let ``Can parse a Struct with numbered fields, defaults, and no optional fields`` () = 
                let testdata = """struct Work {
                    i32 num = 0,
                    i32 num2,
                    Operation op,
                    string comment }"""
                match asInput testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),Some (Ast.IntConstant(0)))
                                                                           Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)
                                                                           Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None)
                                                                           Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[Field(BaseField(BaseType.I32),Identifier(\"num1\"),Some(InstConstant(0)),Field(BaseField(BaseType.I32),Identifier(\"num2\"),None),Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None),Field(BaseField(BaseType.String),Identifier(\"comment\"),None)])))"
        module ``exception`` = 
            [<Test>]
            let ``Can parse an Exception with numbered fields, no defaults or optionals`` () = 
                let testdata = """exception WorkException {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: string comment }"""
                match asInput testdata with
                | Exception (excep,_) ->
                    excep |> should equal (Ast.ExceptionDefinition(Ast.Exception(Ast.Identifier("WorkException"),
                                                                                 [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None))
                                                                                  Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                                  Ast.NumberedField(3,Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                                  Ast.NumberedField(4,Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None))
                                                                                 ])))
                | _ -> failwith "Expecting ExceptionDefinition(Exception(Identifier(\"Work\"),[NumberedField(1,Field(BaseField(BaseType.I32),Identifier(\"num1\"),None)),NumberedField(2,Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),NumberedField(3,Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),NumberedField(4,Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
            // Don't think we need to iterate over all the struct options

        module ``union`` = 
            [<Test>]
            let ``Can parse a Union`` () = 
                let testdata = """union WorkUnion {
                    1: i32 intVal
                    2: string stringVal
                    3: Operation operationVal }"""
                match asInput testdata with
                | Union (union,_) ->
                    union |> should equal (Ast.UnionDefinition(Ast.Union(Ast.Identifier("WorkUnion"),
                                                                         [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("intVal"),None))
                                                                          Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("stringVal"),None))
                                                                          Ast.NumberedField(3,Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("operationVal"),None))
                                                                         ])))
                | _ -> failwith "Expecting UndionDefinition(Union(Identifier(\"WorkUnion\"),NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"intVal\"),None))),NumberedField(2,RequiredField(Field(BaseField(String),Identifier(\"stringVal\"),None))),NumberedField(3,RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"operationVal\"),None)))]))"
            // Don't think we need to iterate over all the struct options

        module ``service`` =
            [<Test>]
            let ``Can parse a service with no inheritance or exceptions`` () =
                let testdata = """service MyService {
                                    void ping(),
                                    i32 add(1:i32 num1,2:i32 num2)
                                  }
                               """
                match asInput testdata with
                | Service (service,_) -> 
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None))
                                                                                              Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],None)))"
            [<Test>]
            let ``Can parse a service with inheritance and no exceptions`` () = 
                let testdata = """service MyService extends AnotherService {
                                      void ping(),
                                      i32 add(1:i32 num1;2:i32 num2)
                                  }
                               """
                match asInput testdata with
                | Service (service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None))
                                                                                              Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                                             ],[])],Some <| Ast.Identifier("AnotherService"))))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],Some (Identifier(\"AnotherService\"))))"
            [<Test>]
            let ``Can parse a service with exceptions`` () = 
                let testdata = """service MyService {
                                      void ping() throws (1:SomeException err1,2:AnotherException err2),
                                      i32 add(1:i32 num1;2:i32 num2)
                                  }
                               """
                match asInput testdata with
                | Service (service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[Ast.NumberedField(1,Ast.Field(Ast.IdentifierField(Ast.Identifier("SomeException")),Ast.Identifier("err1"),None));
                                                                                                                            Ast.NumberedField(2,Ast.Field(Ast.IdentifierField(Ast.Identifier("AnotherException")),Ast.Identifier("err2"),None))]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None))
                                                                                              Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[NumberedField(1,Field(IdentifierField(Identifier(\"SomeException\")),Identifier(\"err1\"),None));NumberedField(2,Field(IdentifierField(Identifier(\"AnotherException\")),Identifier(\"err2\"),None))]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,Field(BaseField(I32),Identifier(\"num1\"),None));NumberedField(2,Field(BaseField(I32),Identifier(\"num2\"),None))],[])],None)))"
            [<Test>]
            let ``Can parse a service with comments`` () = 
                let testdata = """service MyService { // Service definition
                                    /**
                                     * Sends a ping signal to the server
                                     */
                                    void ping()

                                    /**
                                     * Adds two numbers and returns the results
                                     */
                                    i32 add(1:i32 num1;2:i32 num2)
                                }"""
                match asInput testdata with
                | Service(service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None))
                                                                                              Ast.NumberedField(2,Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,Field(BaseField(I32),Identifier(\"num1\"),None));NumberedField(2,Field(BaseField(I32),Identifier(\"num2\"),None))],[])],Some (Identifier(\"AnotherService\"))))"
            [<Test>]
            let ``Can parse a service with oneway (async) functions`` () = 
                let testdata = """service MyAsyncService {
                                    oneway void ping() 
                                  }"""
                match asInput testdata with
                | Service (service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyAsyncService"),
                                                                               [Ast.OnewayFunction(Ast.VoidFunction(Ast.Identifier("ping"),[],[]))],None)))
                | _ -> failwith "Expected ServiceDefinition(Service(Identifier(\"MyAsyncService\"),[OnewayFunction(VoidFunction(Identifier(\"ping\"),[],[]))],None)))"
                                    
    module ``Document`` = 
        open Ast
        [<Test>]
        let ``Can parse simple document`` () = 
            let document = """/**
 * Thirft example DDL
 *
 * Osman Yuksel < yuxel {{|AT|}} sonsuzdongu |-| com >
 */
namespace php Example
service Example{
    // return current time stamp
    string showCurrentTimestamp()
     
    // wait for 10 seconds, but work asynchronously
    oneway void asynchronousJob()
}
"""
            let parsedDocument = Parser.parseDocument document
            match parsedDocument.Headers with
            | [header] ->
                header |> should equal (Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.Php,Ast.Identifier("Example"))))
            | _ -> Assert.Fail("Expecting single header element: NamespaceHeader(Namespace(NamespaceScope.Php,Identifier(\"Example\"))))")

            match parsedDocument.Definitions with
            | [definition] ->
                definition |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("Example"),
                                                                             [Ast.Function(Ast.BaseField(Ast.BaseType.String),Ast.Identifier("showCurrentTimestamp"),[],[])
                                                                              Ast.OnewayFunction(Ast.VoidFunction(Ast.Identifier("asynchronousJob"),[],[]))
                                                                             ],None)))
            | _ -> Assert.Fail("Expecting single definition element: ServiceDefinition(Service(Identifier(\"Example\"),[Function(BaseField(BaseType.String),Identifier(\"showCurrentTimestamp\"),[],[]);OnewayFunction(VoidFunction(Identifier(\"asynchronousJob\"),[],[]))],None)))")

        [<Test>]
        let ``Can parse a slightly larger document`` () = 
            let document = """namespace cpp thrift.example
namespace java thrift.example

enum TweetType {
    TWEET,
    RETWEET = 2,
    DM = 0xa,
    REPLY
}

struct Location {
    1: required double latitude;
    2: required double longitude;
}

struct Tweet {
    1: required i32 userId;
    2: required string userName;
    3: required string text;
    4: optional Location loc;
    5: optional TweetType tweetType = TweetType.TWEET;
    16: optional string language = "english";
}

typedef list<Tweet> TweetList

struct TweetSearchResult {
    1: TweetList tweets;
}

exception TwitterUnavailable {
    1: string message;
}

const i32 MAX_RESULTS = 100;

service Twitter {
    void ping(),
    bool postTweet(1:Tweet tweet) throws (1:TwitterUnavailable unavailable),
    TweetSearchResult searchTweets(1:string query);
    oneway void zip()
}
"""
            let parsed = Parser.parseDocument document
            parsed.Headers.[0] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Cpp,Identifier("thrift.example"))))
            parsed.Headers.[1] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Java,Identifier("thrift.example")))) 

            parsed.Definitions.[0] |> should equal (EnumDefinition(Enum(Identifier("TweetType"),[(0,Identifier("TWEET"));(2,Identifier("RETWEET"));(10,Identifier("DM"));(11,Identifier("REPLY"))])))
            parsed.Definitions.[1] |> should equal (StructDefinition(Struct(Identifier("Location"),[NumberedField(1,RequiredField(Field(BaseField(Double),Identifier("latitude"),None)))
                                                                                                    NumberedField(2,RequiredField(Field(BaseField(Double),Identifier("longitude"),None)))])))
            parsed.Definitions.[2] |> should equal (StructDefinition(Struct(Identifier("Tweet"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier("userId"),None)))
                                                                                                 NumberedField(2,RequiredField(Field(BaseField(String),Identifier("userName"),None)))
                                                                                                 NumberedField(3,RequiredField(Field(BaseField(String),Identifier("text"),None)))
                                                                                                 NumberedField(4,OptionalField(Field(IdentifierField(Identifier("Location")),Identifier("loc"),None)))
                                                                                                 NumberedField(5,OptionalField(Field(IdentifierField(Identifier("TweetType")),Identifier("tweetType"),Some(IdentConstant(Identifier("TweetType.TWEET"))))))
                                                                                                 NumberedField(16,OptionalField(Field(BaseField(String),Identifier("language"),Some(LiteralConstant(StringLiteral("english"))))))
                                                                                                ])))
            parsed.Definitions.[3] |> should equal (TypeDefinition(TypeDef(ContainerDefinition(ContainerType.List(FieldType.IdentifierField(Identifier("Tweet")))),Identifier("TweetList"))))
            parsed.Definitions.[4] |> should equal (StructDefinition(Struct(Identifier("TweetSearchResult"),
                                                                            [NumberedField(1,Field(IdentifierField(Identifier("TweetList")),Identifier("tweets"),None))])))
            parsed.Definitions.[5] |> should equal (ExceptionDefinition(Exception(Identifier("TwitterUnavailable"),
                                                                                  [NumberedField(1,Field(BaseField(String),Identifier("message"),None))])))
            parsed.Definitions.[6] |> should equal (ConstDefinition(Const(BaseField(I32),Identifier("MAX_RESULTS"),IntConstant(100))))
            parsed.Definitions.[7] |> should equal (ServiceDefinition(Service(Identifier("Twitter"),
                                                                              [VoidFunction(Identifier("ping"),[],[])
                                                                               Function(BaseField(Ast.Bool),Identifier("postTweet"),
                                                                                        [NumberedField(1,Field(IdentifierField(Identifier("Tweet")),Identifier("tweet"),None))],
                                                                                        [NumberedField(1,Field(IdentifierField(Identifier("TwitterUnavailable")),Identifier("unavailable"),None))])
                                                                               Function(IdentifierField(Identifier("TweetSearchResult")),Identifier("searchTweets"),
                                                                                        [NumberedField(1,Field(BaseField(String),Identifier("query"),None))],[])
                                                                               OnewayFunction(VoidFunction(Identifier("zip"),[],[]))
                                                                              ],None)))
        [<Test>]
        let ``Can parse the thrift test document from apache thrift project`` () = 
            let document = """/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

namespace c_glib TTest
namespace java thrift.test
namespace cpp thrift.test
namespace rb Thrift.Test
namespace perl ThriftTest
namespace csharp Thrift.Test
namespace js ThriftTest
namespace st ThriftTest
namespace py ThriftTest
namespace py.twisted ThriftTest
namespace go ThriftTest
namespace php ThriftTest
namespace delphi Thrift.Test
namespace cocoa ThriftTest

// Presence of namespaces and sub-namespaces for which there is
// no generator should compile with warnings only
namespace noexist ThriftTest
namespace cpp.noexist ThriftTest

namespace * thrift.test

/**
 * Docstring!
 */
enum Numberz
{
  ONE = 1,
  TWO,
  THREE,
  FIVE = 5,
  SIX,
  EIGHT = 8
}

const Numberz myNumberz = Numberz.ONE;
// the following is expected to fail:
// const Numberz urNumberz = ONE;

typedef i64 UserId

struct Bonk
{
  1: string message,
  2: i32 type
}

typedef map<string,Bonk> MapType

struct Bools {
  1: bool im_true,
  2: bool im_false,
}

struct Xtruct
{
  1:  string string_thing,
  4:  byte   byte_thing,
  9:  i32    i32_thing,
  11: i64    i64_thing
}

struct Xtruct2
{
  1: byte   byte_thing,
  2: Xtruct struct_thing,
  3: i32    i32_thing
}

struct Xtruct3
{
  1:  string string_thing,
  4:  i32    changed,
  9:  i32    i32_thing,
  11: i64    i64_thing
}


struct Insanity
{
  1: map<Numberz, UserId> userMap,
  2: list<Xtruct> xtructs
}

struct CrazyNesting {
  1: string string_field,
  2: optional set<Insanity> set_field,
  3: required list< map<set<i32>,map<i32,set<list<map<Insanity,string>>>>>> list_field,
  4: binary binary_field
}

exception Xception {
  1: i32 errorCode,
  2: string message
}

exception Xception2 {
  1: i32 errorCode,
  2: Xtruct struct_thing
}

struct EmptyStruct {}

struct OneField {
  1: EmptyStruct field
}

service ThriftTest
{
  /**
   * Prints "testVoid()" and returns nothing.
   */
  void         testVoid(),
  
  /**
   * Prints 'testString("%s")' with thing as '%s'
   * @param string thing - the string to print
   * @return string - returns the string 'thing'
   */
  string       testString(1: string thing),
  
  /**
   * Prints 'testByte("%d")' with thing as '%d'
   * @param byte thing - the byte to print
   * @return byte - returns the byte 'thing'
   */
  byte         testByte(1: byte thing),
  
  /**
   * Prints 'testI32("%d")' with thing as '%d'
   * @param i32 thing - the i32 to print
   * @return i32 - returns the i32 'thing'
   */
  i32          testI32(1: i32 thing),
 
  /**
   * Prints 'testI64("%d")' with thing as '%d'
   * @param i64 thing - the i64 to print
   * @return i64 - returns the i64 'thing'
   */
  i64          testI64(1: i64 thing),
  
  /**
   * Prints 'testDouble("%f")' with thing as '%f'
   * @param double thing - the double to print
   * @return double - returns the double 'thing'
   */
  double       testDouble(1: double thing),
  
  /**
   * Prints 'testStruct("{%s}")' where thing has been formatted into a string of comma seperated values
   * @param Xtruct thing - the Xtruct to print
   * @return Xtruct - returns the Xtruct 'thing'
   */
  Xtruct       testStruct(1: Xtruct thing),
  
  /**
   * Prints 'testNest("{%s}")' where thing has been formatted into a string of the nested struct
   * @param Xtruct2 thing - the Xtruct2 to print
   * @return Xtruct2 - returns the Xtruct2 'thing'
   */
  Xtruct2      testNest(1: Xtruct2 thing),
 
  /**
   * Prints 'testMap("{%s")' where thing has been formatted into a string of  'key => value' pairs
   *  seperated by commas and new lines
   * @param map<i32,i32> thing - the map<i32,i32> to print
   * @return map<i32,i32> - returns the map<i32,i32> 'thing'
   */
  map<i32,i32> testMap(1: map<i32,i32> thing),
  
  /**
   * Prints 'testStringMap("{%s}")' where thing has been formatted into a string of  'key => value' pairs
   *  seperated by commas and new lines
   * @param map<string,string> thing - the map<string,string> to print
   * @return map<string,string> - returns the map<string,string> 'thing'
   */
  map<string,string> testStringMap(1: map<string,string> thing),
  
  /**
   * Prints 'testSet("{%s}")' where thing has been formatted into a string of  values
   *  seperated by commas and new lines
   * @param set<i32> thing - the set<i32> to print
   * @return set<i32> - returns the set<i32> 'thing'
   */
  set<i32>     testSet(1: set<i32> thing),
  
  /**
   * Prints 'testList("{%s}")' where thing has been formatted into a string of  values
   *  seperated by commas and new lines
   * @param list<i32> thing - the list<i32> to print
   * @return list<i32> - returns the list<i32> 'thing'
   */
  list<i32>    testList(1: list<i32> thing),
  
  /**
   * Prints 'testEnum("%d")' where thing has been formatted into it's numeric value
   * @param Numberz thing - the Numberz to print
   * @return Numberz - returns the Numberz 'thing'
   */
  Numberz      testEnum(1: Numberz thing),

  /**
   * Prints 'testTypedef("%d")' with thing as '%d'
   * @param UserId thing - the UserId to print
   * @return UserId - returns the UserId 'thing'
   */
  UserId       testTypedef(1: UserId thing),

  /**
   * Prints 'testMapMap("%d")' with hello as '%d'
   * @param i32 hello - the i32 to print
   * @return map<i32,map<i32,i32>> - returns a dictionary with these values:
   *   {-4 => {-4 => -4, -3 => -3, -2 => -2, -1 => -1, }, 4 => {1 => 1, 2 => 2, 3 => 3, 4 => 4, }, }
   */
  map<i32,map<i32,i32>> testMapMap(1: i32 hello),

  /**
   * So you think you've got this all worked, out eh?
   *
   * Creates a the returned map with these values and prints it out:
   *   { 1 => { 2 => argument, 
   *            3 => argument, 
   *          },
   *     2 => { 6 => <empty Insanity struct>, },
   *   }
   * @return map<UserId, map<Numberz,Insanity>> - a map with the above values 
   */
  map<UserId, map<Numberz,Insanity>> testInsanity(1: Insanity argument),

  /**
   * Prints 'testMulti()'
   * @param byte arg0 - 
   * @param i32 arg1 - 
   * @param i64 arg2 - 
   * @param map<i16, string> arg3 - 
   * @param Numberz arg4 - 
   * @param UserId arg5 - 
   * @return Xtruct - returns an Xtruct with string_thing = "Hello2, byte_thing = arg0, i32_thing = arg1
   *    and i64_thing = arg2
   */
  Xtruct testMulti(1: byte arg0, 2: i32 arg1, 3: i64 arg2, 4: map<i16, string> arg3, 5: Numberz arg4, 6: UserId arg5),

  /**
   * Print 'testException(%s)' with arg as '%s'
   * @param string arg - a string indication what type of exception to throw
   * if arg == "Xception" throw Xception with errorCode = 1001 and message = arg
   * elsen if arg == "TException" throw TException
   * else do not throw anything
   */
  void testException(1: string arg) throws(1: Xception err1),

  /**
   * Print 'testMultiException(%s, %s)' with arg0 as '%s' and arg1 as '%s'
   * @param string arg - a string indication what type of exception to throw
   * if arg0 == "Xception" throw Xception with errorCode = 1001 and message = "This is an Xception"
   * elsen if arg0 == "Xception2" throw Xception2 with errorCode = 2002 and message = "This is an Xception2"
   * else do not throw anything
   * @return Xtruct - an Xtruct with string_thing = arg1
   */
  Xtruct testMultiException(1: string arg0, 2: string arg1) throws(1: Xception err1, 2: Xception2 err2)

  /**
   * Print 'testOneway(%d): Sleeping...' with secondsToSleep as '%d'
   * sleep 'secondsToSleep'
   * Print 'testOneway(%d): done sleeping!' with secondsToSleep as '%d'
   * @param i32 secondsToSleep - the number of seconds to sleep
   */
  oneway void testOneway(1:i32 secondsToSleep)
}

service SecondService
{
  void blahBlah()
  /**
   * Prints 'testString("%s")' with thing as '%s'
   * @param string thing - the string to print
   * @return string - returns the string 'thing'
   */
  string       secondtestString(1: string thing),
}

struct VersioningTestV1 {
       1: i32 begin_in_both,
       3: string old_string,
       12: i32 end_in_both
}

struct VersioningTestV2 {
       1: i32 begin_in_both,

       2: i32 newint,
       3: byte newbyte,
       4: i16 newshort,
       5: i64 newlong,
       6: double newdouble
       7: Bonk newstruct,
       8: list<i32> newlist,
       9: set<i32> newset,
       10: map<i32, i32> newmap,
       11: string newstring,
       12: i32 end_in_both
}

struct ListTypeVersioningV1 {
       1: list<i32> myints;
       2: string hello;
}

struct ListTypeVersioningV2 {
       1: list<string> strings;
       2: string hello;
}

struct GuessProtocolStruct {
  7: map<string,string> map_field,
}

struct LargeDeltas {
  1: Bools b1,
  10: Bools b10,
  100: Bools b100,
  500: bool check_true,
  1000: Bools b1000,
  1500: bool check_false,
  2000: VersioningTestV2 vertwo2000,
  2500: set<string> a_set2500,
  3000: VersioningTestV2 vertwo3000,
  4000: list<i32> big_numbers
}

struct NestedListsI32x2 {
  1: list<list<i32>> integerlist
}
struct NestedListsI32x3 {
  1: list<list<list<i32>>> integerlist
}
struct NestedMixedx2 {
  1: list<set<i32>> int_set_list
  2: map<i32,set<string>> map_int_strset
  3: list<map<i32,set<string>>> map_int_strset_list
}
struct ListBonks {
  1: list<Bonk> bonk
}
struct NestedListsBonk {
  1: list<list<list<Bonk>>> bonk
}

struct BoolTest {
  1: optional bool b = true;
  2: optional string s = "true";
}

struct StructA {
  1: required string s;
}

struct StructB {
  1: optional StructA aa;
  2: required StructA ab;
}"""
            let doc = Parser.parseDocument document
            doc.Headers.[0]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.C_Glib,Identifier("TTest"))))
            doc.Headers.[1]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Java,Identifier("thrift.test"))))
            doc.Headers.[2]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Cpp,Identifier("thrift.test"))))
            doc.Headers.[3]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Rb,Identifier("Thrift.Test"))))
            doc.Headers.[4]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Perl,Identifier("ThriftTest"))))
            doc.Headers.[5]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.CSharp,Identifier("Thrift.Test"))))
            doc.Headers.[6]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Javascript,Identifier("ThriftTest"))))
            doc.Headers.[7]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Smalltalk,Identifier("ThriftTest"))))
            doc.Headers.[8]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Py,Identifier("ThriftTest"))))
            doc.Headers.[9]  |> should equal (NamespaceHeader(Namespace(NamespaceScope.Py_Twisted,Identifier("ThriftTest"))))
            doc.Headers.[10] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Go,Identifier("ThriftTest"))))
            doc.Headers.[11] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Php,Identifier("ThriftTest"))))
            doc.Headers.[12] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Delphi,Identifier("Thrift.Test"))))
            doc.Headers.[13] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Cocoa,Identifier("ThriftTest"))))

            doc.Headers.[14] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Other("noexist"),Identifier("ThriftTest"))))
            doc.Headers.[15] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Other("cpp.noexist"),Identifier("ThriftTest"))))

            doc.Headers.[16] |> should equal (NamespaceHeader(Namespace(NamespaceScope.Any,Identifier("thrift.test"))))

            doc.Definitions.[0]  |> should equal (EnumDefinition(Enum(Identifier("Numberz"),[(1,Identifier("ONE"))
                                                                                             (2,Identifier("TWO"))
                                                                                             (3,Identifier("THREE"))
                                                                                             (5,Identifier("FIVE"))
                                                                                             (6,Identifier("SIX"))
                                                                                             (8,Identifier("EIGHT"))
                                                                                             ])))
            doc.Definitions.[1]  |> should equal (ConstDefinition(Const(IdentifierField(Identifier("Numberz")),Identifier("myNumberz"),ConstantValue.IdentConstant(Identifier("Numberz.ONE")))))

            doc.Definitions.[2]  |> should equal (TypeDefinition(TypeDef(BaseDefinition(I64),Identifier("UserId"))))

            doc.Definitions.[3]  |> should equal (StructDefinition(Struct(Identifier("Bonk"),
                                                                          [NumberedField(1,Field(BaseField(String),Identifier("message"),None))
                                                                           NumberedField(2,Field(BaseField(I32),Identifier("type"),None))
                                                                          ])))

            doc.Definitions.[4]  |> should equal (TypeDefinition(TypeDef(ContainerDefinition(ContainerType.Map(BaseField(String),IdentifierField(Identifier("Bonk")))),Identifier("MapType"))))

            doc.Definitions.[5]  |> should equal (StructDefinition(Struct(Identifier("Bools"),
                                                                          [NumberedField(1,Field(BaseField(Bool),Identifier("im_true"),None))
                                                                           NumberedField(2,Field(BaseField(Bool),Identifier("im_false"),None))
                                                                          ])))
            doc.Definitions.[6]  |> should equal (StructDefinition(Struct(Identifier("Xtruct"),
                                                                          [NumberedField(1,Field(BaseField(String),Identifier("string_thing"),None))
                                                                           NumberedField(4,Field(BaseField(Byte),Identifier("byte_thing"),None))
                                                                           NumberedField(9,Field(BaseField(I32),Identifier("i32_thing"),None))
                                                                           NumberedField(11,Field(BaseField(I64),Identifier("i64_thing"),None))
                                                                          ])))
            doc.Definitions.[7]  |> should equal (StructDefinition(Struct(Identifier("Xtruct2"),
                                                                          [NumberedField(1,Field(BaseField(Byte),Identifier("byte_thing"),None))
                                                                           NumberedField(2,Field(IdentifierField(Identifier("Xtruct")),Identifier("struct_thing"),None))
                                                                           NumberedField(3,Field(BaseField(I32),Identifier("i32_thing"),None))
                                                                          ])))
            doc.Definitions.[8]  |> should equal (StructDefinition(Struct(Identifier("Xtruct3"),
                                                                          [NumberedField(1,Field(BaseField(String),Identifier("string_thing"),None))
                                                                           NumberedField(4,Field(BaseField(I32),Identifier("changed"),None))
                                                                           NumberedField(9,Field(BaseField(I32),Identifier("i32_thing"),None))
                                                                           NumberedField(11,Field(BaseField(I64),Identifier("i64_thing"),None))
                                                                          ])))
            doc.Definitions.[9]  |> should equal (StructDefinition(Struct(Identifier("Insanity"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.Map(IdentifierField(Identifier("Numberz")),IdentifierField(Identifier("UserId")))),Identifier("userMap"),None))
                                                                           NumberedField(2,Field(ContainerField(ContainerType.List(IdentifierField(Identifier("Xtruct")))),Identifier("xtructs"),None))
                                                                          ])))
            doc.Definitions.[10] |> should equal (StructDefinition(Struct(Identifier("CrazyNesting"),
                                                                          [NumberedField(1,Field(BaseField(String),Identifier("string_field"),None))
                                                                           NumberedField(2,OptionalField(Field(ContainerField(ContainerType.Set(IdentifierField(Identifier("Insanity")))),Identifier("set_field"),None)))
                                                                           NumberedField(3,RequiredField(Field(
                                                                                                            ContainerField(
                                                                                                                ContainerType.List(
                                                                                                                    ContainerField(
                                                                                                                        ContainerType.Map(
                                                                                                                            ContainerField(
                                                                                                                                ContainerType.Set(BaseField(I32))
                                                                                                                            ),                                                                                                                        
                                                                                                                            ContainerField(
                                                                                                                                ContainerType.Map(
                                                                                                                                    BaseField(I32),
                                                                                                                                    ContainerField(
                                                                                                                                        ContainerType.Set(
                                                                                                                                            ContainerField(
                                                                                                                                                ContainerType.List(
                                                                                                                                                    ContainerField(
                                                                                                                                                        ContainerType.Map(
                                                                                                                                                            IdentifierField(
                                                                                                                                                                Identifier("Insanity")),
                                                                                                                                                            BaseField(String)
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                            ),Identifier("list_field"),None)))
                                                                           NumberedField(4,Field(BaseField(Binary),Identifier("binary_field"),None))
                                                                          ])))
            doc.Definitions.[11] |> should equal (ExceptionDefinition(Exception(Identifier("Xception"),
                                                                                [NumberedField(1,Field(BaseField(I32),Identifier("errorCode"),None))
                                                                                 NumberedField(2,Field(BaseField(String),Identifier("message"),None))
                                                                                ])))
            doc.Definitions.[12] |> should equal (ExceptionDefinition(Exception(Identifier("Xception2"),
                                                                                [NumberedField(1,Field(BaseField(I32),Identifier("errorCode"),None))
                                                                                 NumberedField(2,Field(IdentifierField(Identifier("Xtruct")),Identifier("struct_thing"),None))
                                                                                ])))
            doc.Definitions.[13] |> should equal (StructDefinition(Struct(Identifier("EmptyStruct"),[])))
            doc.Definitions.[14] |> should equal (StructDefinition(Struct(Identifier("OneField"),
                                                                          [NumberedField(1,Field(IdentifierField(Identifier("EmptyStruct")),Identifier("field"),None))])))

            doc.Definitions.[15] |> should equal (ServiceDefinition(Service(Identifier("ThriftTest"),
                                                                            [VoidFunction(Identifier("testVoid"),[],[])
                                                                             Function(BaseField(String),Identifier("testString"),
                                                                                      [NumberedField(1,Field(BaseField(String),Identifier("thing"),None))],[])
                                                                             Function(BaseField(Byte),Identifier("testByte"),
                                                                                      [NumberedField(1,Field(BaseField(Byte),Identifier("thing"),None))],[])
                                                                             Function(BaseField(I32),Identifier("testI32"),
                                                                                      [NumberedField(1,Field(BaseField(I32),Identifier("thing"),None))],[])
                                                                             Function(BaseField(I64),Identifier("testI64"),
                                                                                      [NumberedField(1,Field(BaseField(I64),Identifier("thing"),None))],[])
                                                                             Function(BaseField(Double),Identifier("testDouble"),
                                                                                      [NumberedField(1,Field(BaseField(Double),Identifier("thing"),None))],[])
                                                                             Function(IdentifierField(Identifier("Xtruct")),Identifier("testStruct"),
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("Xtruct")),Identifier("thing"),None))],[])
                                                                             Function(IdentifierField(Identifier("Xtruct2")),Identifier("testNest"),
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("Xtruct2")),Identifier("thing"),None))],[])
                                                                             Function(ContainerField(ContainerType.Map(BaseField(I32),BaseField(I32))),Identifier("testMap"),
                                                                                      [NumberedField(1,Field(ContainerField(ContainerType.Map(BaseField(I32),BaseField(I32))),Identifier("thing"),None))],[])
                                                                             Function(ContainerField(ContainerType.Map(BaseField(String),BaseField(String))),Identifier("testStringMap"),
                                                                                      [NumberedField(1,Field(ContainerField(ContainerType.Map(BaseField(String),BaseField(String))),Identifier("thing"),None))],[])
                                                                             Function(ContainerField(ContainerType.Set(BaseField(I32))),Identifier("testSet"),
                                                                                      [NumberedField(1,Field(ContainerField(ContainerType.Set(BaseField(I32))),Identifier("thing"),None))],[])
                                                                             Function(ContainerField(ContainerType.List(BaseField(I32))),Identifier("testList"),
                                                                                      [NumberedField(1,Field(ContainerField(ContainerType.List(BaseField(I32))),Identifier("thing"),None))],[])
                                                                             Function(IdentifierField(Identifier("Numberz")),Identifier("testEnum"),
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("Numberz")),Identifier("thing"),None))],[])
                                                                             Function(IdentifierField(Identifier("UserId")),Identifier("testTypedef"),
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("UserId")),Identifier("thing"),None))],[])
                                                                             Function(ContainerField(ContainerType.Map(BaseField(I32),ContainerField(ContainerType.Map(BaseField(I32),BaseField(I32))))),Identifier("testMapMap"),
                                                                                      [NumberedField(1,Field(BaseField(I32),Identifier("hello"),None))],[])
                                                                             Function(ContainerField(ContainerType.Map(IdentifierField(Identifier("UserId")),ContainerField(ContainerType.Map(IdentifierField(Identifier("Numberz")),IdentifierField(Identifier("Insanity")))))),Identifier("testInsanity"),
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("Insanity")),Identifier("argument"),None))],[])
                                                                             Function(IdentifierField(Identifier("Xtruct")),Identifier("testMulti"),
                                                                                      [NumberedField(1,Field(BaseField(Byte),Identifier("arg0"),None))
                                                                                       NumberedField(2,Field(BaseField(I32),Identifier("arg1"),None))
                                                                                       NumberedField(3,Field(BaseField(I64),Identifier("arg2"),None))
                                                                                       NumberedField(4,Field(ContainerField(ContainerType.Map(BaseField(I16),BaseField(String))),Identifier("arg3"),None))
                                                                                       NumberedField(5,Field(IdentifierField(Identifier("Numberz")),Identifier("arg4"),None))
                                                                                       NumberedField(6,Field(IdentifierField(Identifier("UserId")),Identifier("arg5"),None))
                                                                                      ],[])
                                                                             VoidFunction(Identifier("testException"),
                                                                                          [NumberedField(1,Field(BaseField(String),Identifier("arg"),None))],
                                                                                          [NumberedField(1,Field(IdentifierField(Identifier("Xception")),Identifier("err1"),None))])
                                                                             Function(IdentifierField(Identifier("Xtruct")),Identifier("testMultiException"),
                                                                                      [NumberedField(1,Field(BaseField(String),Identifier("arg0"),None))
                                                                                       NumberedField(2,Field(BaseField(String),Identifier("arg1"),None))
                                                                                      ],
                                                                                      [NumberedField(1,Field(IdentifierField(Identifier("Xception")),Identifier("err1"),None))
                                                                                       NumberedField(2,Field(IdentifierField(Identifier("Xception2")),Identifier("err2"),None))
                                                                                      ])
                                                                             OnewayFunction(VoidFunction(Identifier("testOneway"),
                                                                                                         [NumberedField(1,Field(BaseField(I32),Identifier("secondsToSleep"),None))],[]))
                                                                            ],None)))
            doc.Definitions.[16] |> should equal (ServiceDefinition(Service(Identifier("SecondService"),
                                                                            [VoidFunction(Identifier("blahBlah"),[],[])
                                                                             Function(BaseField(String),Identifier("secondtestString"),
                                                                                                        [NumberedField(1,Field(BaseField(String),Identifier("thing"),None))],[])
                                                                            ],None)))
            doc.Definitions.[17] |> should equal (StructDefinition(Struct(Identifier("VersioningTestV1"),
                                                                          [NumberedField(1,Field(BaseField(I32),Identifier("begin_in_both"),None))
                                                                           NumberedField(3,Field(BaseField(String),Identifier("old_string"),None))
                                                                           NumberedField(12,Field(BaseField(I32),Identifier("end_in_both"),None))
                                                                          ])))
            doc.Definitions.[18] |> should equal (StructDefinition(Struct(Identifier("VersioningTestV2"),
                                                                          [NumberedField(1,Field(BaseField(I32),Identifier("begin_in_both"),None))
                                                                           NumberedField(2,Field(BaseField(I32),Identifier("newint"),None))
                                                                           NumberedField(3,Field(BaseField(Byte),Identifier("newbyte"),None))
                                                                           NumberedField(4,Field(BaseField(I16),Identifier("newshort"),None))
                                                                           NumberedField(5,Field(BaseField(I64),Identifier("newlong"),None))
                                                                           NumberedField(6,Field(BaseField(Double),Identifier("newdouble"),None))
                                                                           NumberedField(7,Field(IdentifierField(Identifier("Bonk")),Identifier("newstruct"),None))
                                                                           NumberedField(8,Field(ContainerField(ContainerType.List(BaseField(I32))),Identifier("newlist"),None))
                                                                           NumberedField(9,Field(ContainerField(ContainerType.Set(BaseField(I32))),Identifier("newset"),None))
                                                                           NumberedField(10,Field(ContainerField(ContainerType.Map(BaseField(I32),BaseField(I32))),Identifier("newmap"),None))
                                                                           NumberedField(11,Field(BaseField(String),Identifier("newstring"),None))
                                                                           NumberedField(12,Field(BaseField(I32),Identifier("end_in_both"),None))
                                                                          ])))
            doc.Definitions.[19] |> should equal (StructDefinition(Struct(Identifier("ListTypeVersioningV1"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(BaseField(I32))),Identifier("myints"),None))
                                                                           NumberedField(2,Field(BaseField(String),Identifier("hello"),None))
                                                                          ])))
            doc.Definitions.[20] |> should equal (StructDefinition(Struct(Identifier("ListTypeVersioningV2"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(BaseField(String))),Identifier("strings"),None))
                                                                           NumberedField(2,Field(BaseField(String),Identifier("hello"),None))
                                                                          ])))
            doc.Definitions.[21] |> should equal (StructDefinition(Struct(Identifier("GuessProtocolStruct"),
                                                                          [NumberedField(7,Field(ContainerField(ContainerType.Map(BaseField(String),BaseField(String))),Identifier("map_field"),None))
                                                                          ])))
            doc.Definitions.[22] |> should equal (StructDefinition(Struct(Identifier("LargeDeltas"),
                                                                          [NumberedField(1,Field(IdentifierField(Identifier("Bools")),Identifier("b1"),None))
                                                                           NumberedField(10,Field(IdentifierField(Identifier("Bools")),Identifier("b10"),None))
                                                                           NumberedField(100,Field(IdentifierField(Identifier("Bools")),Identifier("b100"),None))
                                                                           NumberedField(500,Field(BaseField(Bool),Identifier("check_true"),None))
                                                                           NumberedField(1000,Field(IdentifierField(Identifier("Bools")),Identifier("b1000"),None))
                                                                           NumberedField(1500,Field(BaseField(Bool),Identifier("check_false"),None))
                                                                           NumberedField(2000,Field(IdentifierField(Identifier("VersioningTestV2")),Identifier("vertwo2000"),None))
                                                                           NumberedField(2500,Field(ContainerField(ContainerType.Set(BaseField(String))),Identifier("a_set2500"),None))
                                                                           NumberedField(3000,Field(IdentifierField(Identifier("VersioningTestV2")),Identifier("vertwo3000"),None))
                                                                           NumberedField(4000,Field(ContainerField(ContainerType.List(BaseField(I32))),Identifier("big_numbers"),None))
                                                                          ])))
            doc.Definitions.[23] |> should equal (StructDefinition(Struct(Identifier("NestedListsI32x2"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(ContainerField(ContainerType.List(BaseField(I32))))),Identifier("integerlist"),None))
                                                                          ])))
            doc.Definitions.[24] |> should equal (StructDefinition(Struct(Identifier("NestedListsI32x3"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(ContainerField(ContainerType.List(ContainerField(ContainerType.List(BaseField(I32))))))),Identifier("integerlist"),None))
                                                                          ])))
            doc.Definitions.[25] |> should equal (StructDefinition(Struct(Identifier("NestedMixedx2"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(ContainerField(ContainerType.Set(BaseField(I32))))),Identifier("int_set_list"),None))
                                                                           NumberedField(2,Field(ContainerField(ContainerType.Map(BaseField(I32),ContainerField(ContainerType.Set(BaseField(String))))),Identifier("map_int_strset"),None))
                                                                           NumberedField(3,Field(ContainerField(ContainerType.List(ContainerField(ContainerType.Map(BaseField(I32),ContainerField(ContainerType.Set(BaseField(String))))))),Identifier("map_int_strset_list"),None))
                                                                          ])))
            doc.Definitions.[26] |> should equal (StructDefinition(Struct(Identifier("ListBonks"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(IdentifierField(Identifier("Bonk")))),Identifier("bonk"),None))
                                                                          ])))
            doc.Definitions.[27] |> should equal (StructDefinition(Struct(Identifier("NestedListsBonk"),
                                                                          [NumberedField(1,Field(ContainerField(ContainerType.List(ContainerField(ContainerType.List(ContainerField(ContainerType.List(IdentifierField(Identifier("Bonk")))))))),Identifier("bonk"),None))
                                                                          ])))
            doc.Definitions.[28] |> should equal (StructDefinition(Struct(Identifier("BoolTest"),
                                                                          [NumberedField(1,OptionalField(Field(BaseField(Bool),Identifier("b"),Some(ConstantValue.LiteralConstant(BoolLiteral(true))))))
                                                                           NumberedField(2,OptionalField(Field(BaseField(String),Identifier("s"),Some(ConstantValue.LiteralConstant(StringLiteral("true"))))))
                                                                          ])))
            doc.Definitions.[29] |> should equal (StructDefinition(Struct(Identifier("StructA"),
                                                                          [NumberedField(1,RequiredField(Field(BaseField(String),Identifier("s"),None)))
                                                                          ])))
            doc.Definitions.[30] |> should equal (StructDefinition(Struct(Identifier("StructB"),
                                                                          [NumberedField(1,OptionalField(Field(IdentifierField(Identifier("StructA")),Identifier("aa"),None)))
                                                                           NumberedField(2,RequiredField(Field(IdentifierField(Identifier("StructA")),Identifier("ab"),None)))
                                                                          ])))