module Miser.``Parser Tests``
    open NUnit.Framework
    open FsUnit
    open Parser
    
    module ``Comments`` =

        [<Test>]
        let ``Can parse single in-line comment starting with // from single line`` () = 
            let testData = "// This is a single in-line comment with no additional lines"
            match asCharList testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment with no additional lines")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment starting with # from single line`` () = 
            let testData = "# This is a single in-line comment starting with #"
            match asCharList testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment starting with #")
            | _ -> Assert.Fail()
        [<Test>]
        let ``Can parse single in-line comment with multiple lines`` () = 
            let testData = """// This is a single in-line comment
This is something else"""
            match asCharList testData with
            | InlineComment (comment,rest) -> comment |> should equal (Ast.Comment " This is a single in-line comment");rest |> should equal "This is something else"
            | _ -> Assert.Fail()

        [<Test>]
        let ``Can parse a multi-line block comment`` () = 
            let testData = """/* This is a
multi-line block style
comment */"""
            match asCharList testData with
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
            match testdata with
            | DocComment (comment,_) -> comment |> should equal (Ast.DocComment("\r\n * This is a sample doc comment\r\n "))
            | _ -> Assert.Fail()
    module ``Identifiers`` =
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
            let testData = "\"This test mustn't fail\""
            match testData with
            | StringLiteral (literal,_) -> literal |> should equal (Ast.StringLiteral "This test mustn't fail")
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
        // These were in the sample, but not the IDL definition
        // c_glib
        [<Test>]
        let ``Namespace can be defined for C_Glib scope using 'c_glib'`` () =
            let testData = "namespace c_glib MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.C_Glib,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // py.twisted
        [<Test>]
        let ``Namespace can be defined for Py_Twisted scope using 'py.twisted'`` () =
            let testData = "namespace py.twisted MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Py_Twisted,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // go
        [<Test>]
        let ``Namespace can be defined for Go scope using 'go'`` () =
            let testData = "namespace go MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Go,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // delphi
        [<Test>]
        let ``Namespace can be defined for Delphi scope using 'delphi'`` () =
            let testData = "namespace delphi MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Delphi,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // js
        [<Test>]
        let ``Namespace can be defined for Javascript scope using 'js'`` () =
            let testData = "namespace js MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Javascript,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // st
        [<Test>]
        let ``Namespace can be defined for Smalltalk scope using 'st'`` () =
            let testData = "namespace st MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Smalltalk,Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
        // Others should compile with warnings
        [<Test>]
        let ``Namespace can be defined for Other scope using a non-registered designator`` () =
            let testData = "namespace someother MyNamespace.Net"
            match testData with
            | ThriftNamespace (nSpace,_) -> nSpace |> should equal (Ast.Namespace(Ast.NamespaceScope.Other("someother"),Ast.Identifier "MyNamespace.Net"))
            | _ -> Assert.Fail()
    module ``Header`` =         
        [<Test>]
        let ``Header can contain just an include`` () = 
            let testData = """import "somefile.thrift" """
            match testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "somefile.thrift")))]
            | _ -> Assert.Fail()
        
        [<Test>]
        let ``Header can contain multiple includes`` () = 
            let testData = """import "somefile.thrift" 
                              import "someotherfile.thrift" """
            match testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "somefile.thrift")));
                                                               (Ast.IncludeHeader(Ast.Include(Ast.StringLiteral "someotherfile.thrift")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain just a namespace`` () = 
            let testData = "namespace csharp SomeNamespace.Net"
            match testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "SomeNamespace.Net")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain multiple Namespaces`` () = 
            let testData = """namespace csharp SomeNamespace.Net
                              namespace java org.java.something"""
            match testData with
            | ThriftHeader (header,_) -> header |> should equal [(Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.CSharp,Ast.Identifier "SomeNamespace.Net")))
                                                                 (Ast.NamespaceHeader(Ast.Namespace(Ast.NamespaceScope.Java,Ast.Identifier "org.java.something")))]
            | _ -> Assert.Fail()

        [<Test>]
        let ``Header can contain includes and namespaces`` () = 
            let testData = """import "somefile.thrift"
                              import "anotherfile.thrift"

                              namespace csharp SomeNamespace.Net
                              namespace java org.java.something"""

            match testData with
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
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Bool
                | _ -> Assert.Fail()
            
            [<Test>]
            let ``Can parse a byte base type`` () = 
                let testdata = "byte"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Byte
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int16 base type`` () = 
                let testdata = "i16"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I16
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int32 base type`` () = 
                let testdata = "i32"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I32
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int32 base type using 'int'`` () = 
                let testdata = "int"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I32
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a int64 base type`` () = 
                let testdata = "i64"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.I64
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a double base type`` () = 
                let testdata = "double"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Double
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a string base type`` () = 
                let testdata = "string"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.String
                | _ -> Assert.Fail()

            [<Test>]
            let ``Can parse a binary base type`` () = 
                let testdata = "binary"
                match testdata with
                | BaseType (baseType,_) -> baseType |> should equal Ast.BaseType.Binary
                | _ -> Assert.Fail()
        module ``Container Types`` = 
            [<Test>]
            let ``Can parse a list type`` () =
                let testdata = "list<i32>"
                match testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.List(Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting List(BaseField(I32))"
            [<Test>]
            let ``Can parse a set type`` () =
                let testdata = "set<i32>"
                match testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.Set(Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting Set(BaseField(I32))"
            [<Test>]
            let ``Can parse a map type`` () =
                let testdata = "map<string,i32>"
                match testdata with
                | ContainerType (ctype,_) -> ctype |> should equal (Ast.ContainerType.Map(Ast.FieldType.BaseField(Ast.BaseType.String),Ast.FieldType.BaseField(Ast.BaseType.I32)))
                | _ -> failwith "Expecting Map(BaseField(String),BaseField(I32))"
        module ``Definition Types`` = 
            [<Test>]
            let ``Can parse a definition type based on a BaseType`` () = 
                match "i32" with
                | DefinitionType (dtype,_) -> dtype |> should equal (Ast.DefinitionType.BaseDefinition(Ast.BaseType.I32))
                | _ -> failwith "Expecting DefinitionType(BaseField(String),BaseField(I32))"
            [<Test>]
            let ``Can parse a definition type based on a ContainerType`` () = 
                let testdata = "list<i32>"
                match testdata with
                | DefinitionType(dtype,_) -> dtype |> should equal (Ast.DefinitionType.ContainerDefinition(Ast.ContainerType.List(Ast.FieldType.BaseField(Ast.BaseType.I32))))
                | _ -> failwith "Expecting DefinitionType(ContainerDefinition(Set(BaseField(I32))))"
        module ``Field Types`` = 
            [<Test>]
            let ``Can parse an Identifier as a FieldType`` () = 
                let testdata = "MyIdentifier"
                match testdata with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.IdentifierField(Ast.Identifier("MyIdentifier")))
                | _ -> failwith "Expecting FieldType(Identifier(\"MyIdentifier\"))"
            [<Test>]
            let ``Can parse a BaseType as a FieldType`` () = 
                match "string" with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.BaseField(Ast.BaseType.String))
                | _ -> failwith "Expecting FieldType(BaseType.String)"
            [<Test>]
            let ``Can parse a ContainerType as a FieldType`` () = 
                let testdata = "map<string,string>"
                match testdata with
                | FieldType(ftype,_) -> ftype |> should equal (Ast.FieldType.ContainerField(Ast.ContainerType.Map(Ast.FieldType.BaseField(Ast.BaseType.String),Ast.FieldType.BaseField(Ast.BaseType.String))))
                | _ -> failwith "Expecting FieldType(ContainerType.Map(FieldType(BaseType.String),FieldType(BaseType.String)))"
    module ``Definitions`` =
        module ``Constants`` = 
            [<Test>]
            let ``Can parse Constant with string literal value`` () = 
                let testdata = "const string MY_STRING \"mystringliteral\""
                match testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.String),Ast.Identifier("MY_STRING"),Ast.ConstantValue.LiteralConstant(Ast.StringLiteral("mystringliteral")))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseTpe.String),Identifier(\"MY_STRING\"),ConstantValue.LiteralConstant(\"mystringliteral\"))"
            [<Test>]
            let ``Can parse Constant with integer literal value`` () = 
                let testdata = "const i32 MY_INT32 12345"
                match testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.I32),Ast.Identifier("MY_INT32"),Ast.ConstantValue.IntConstant(12345))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.I32),Identifier(\"MY_INT32\"),ConstantValue.IntConstant(12345))"
            [<Test>]
            let ``Can parse Constant with negative integer literal value`` () = 
                let testdata = "const i32 MY_NEG_INT32 -12345"
                match testdata with
                | Constant(constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.I32),Ast.Identifier("MY_NEG_INT32"),Ast.ConstantValue.IntConstant(-12345))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.I32),Identifier(\"MY_NEG_INT32\"),ConstantValue.IntConstant(-12345))"
            [<Test>]
            let ``Can parse Constant with double literal value`` () = 
                let testdata = "const double MY_DOUBLE 123.45"
                match testdata with
                | Constant (constant,_) -> constant |> should equal (Ast.ConstDefinition(Ast.Const(Ast.BaseField(Ast.BaseType.Double),Ast.Identifier("MY_DOUBLE"),Ast.ConstantValue.DoubleConstant(123.45))))
                | _ -> failwith "Expecting ConstDefinition(BaseField(BaseType.Double),Identifier(\"MY_DOUBLE\"),ConstantValue.DoubleConstant(123.45))"
            [<Test>]
            let ``Can parse Constant with literal List value`` () =
                let testdata = "const list<i32> MY_LIST [1,2,3,4,5]"
                match testdata with
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
                let testdata = "const map<string,i32> MY_MAP {\"one\":1;\"two\":2;\"three\":3}"
                match testdata with
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
                match testData with
                | TypeDef (def,_) -> def |> should equal (Ast.TypeDef(Ast.BaseDefinition(Ast.BaseType.I32),Ast.Identifier("MyInteger")))
                | _ -> Assert.Fail()
        module ``enum`` = 
            [<Test>]
            let ``Can parse an enum without integer value assignments`` () = 
                let testdata = """enum MyEnum {
                    FIRST,
                    SECOND,
                    THIRD }"""
                match testdata with
                | Enum (enum,_) -> enum |> should equal (Ast.EnumDefinition(Ast.Enum(Ast.Identifier("MyEnum"),[(0,Ast.Identifier("FIRST"));(1,Ast.Identifier("SECOND"));(2,Ast.Identifier("THIRD"))])))
                | _ -> failwith "Expecting EnumDefinition(Enum(Identifier(\"MyEnum\"),[(0,Identifier(\"FIRST\"));(1,Identifier(\"SECOND\"));(2,Identifier(\"THIRD\"))]))"
            [<Test>]
            let ``Can parse an enum with integer value assignments`` () = 
                let testdata = """enum MyEnum {
                    FIRST = 1,
                    SECOND = 2,
                    THIRD = 4 }"""
                match testdata with
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
                match testdata with
                | Struct (strct,_) -> 
                    printf "%A" strct
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None)))
                                                                           Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                           Ast.NumberedField(3,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None)))
                                                                           Ast.NumberedField(4,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[NumberedField(1,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num1\"),None))),NumberedField(2,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num2\"),None))),NumberedField(3,RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None))),NumberedField(4,RequiredField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None)))])))"
            [<Test>]
            let ``Can parse a Struct with numbered fields, no defaults, and optional fields`` () = 
                let testdata = """struct Work {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: optional string comment }"""
                match testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None)))
                                                                           Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                           Ast.NumberedField(3,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None)))
                                                                           Ast.NumberedField(4,Ast.OptionalField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[NumberedField(1,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num1\"),None))),NumberedField(2,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num2\"),None))),NumberedField(3,RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None))),NumberedField(4,OptionalField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None)))])))"

            [<Test>]
            let ``Can parse a Struct with unnumbered fields, no defaults, and no optional fields`` () = 
                let testdata = """struct Work {
                    i32 num,
                    i32 num2,
                    Operation op,
                    string comment }"""
                match testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None))
                                                                           Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                           Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                           Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num1\"),None)),RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),OptionalField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
            [<Test>]
            let ``Can parse a Struct with numbered fields, defaults, and no optional fields`` () = 
                let testdata = """struct Work {
                    i32 num = 0,
                    i32 num2,
                    Operation op,
                    string comment }"""
                match testdata with
                | Struct (strct,_) ->
                    strct |> should equal (Ast.StructDefinition(Ast.Struct(Ast.Identifier("Work"),
                                                                          [Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),Some (Ast.IntConstant(0))))
                                                                           Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None))
                                                                           Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None))
                                                                           Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None))
                                                                          ])))
                | _ -> failwith "Expecting StructDefinition(Struct(Identifier(\"Work\"),[RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num1\"),Some(InstConstant(0))),RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num2\"),None)),RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None)),OptionalField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None))])))"
        module ``exception`` = 
            [<Test>]
            let ``Can parse an Exception with numbered fields, no defaults or optionals`` () = 
                let testdata = """exception WorkException {
                    1: i32 num,
                    2: i32 num2,
                    3: Operation op,
                    4: string comment }"""
                match testdata with
                | Exception (excep,_) ->
                    excep |> should equal (Ast.ExceptionDefinition(Ast.Exception(Ast.Identifier("WorkException"),
                                                                                 [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num"),None)))
                                                                                  Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                                  Ast.NumberedField(3,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("op"),None)))
                                                                                  Ast.NumberedField(4,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("comment"),None)))
                                                                                 ])))
                | _ -> failwith "Expecting ExceptionDefinition(Exception(Identifier(\"Work\"),[NumberedField(1,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num1\"),None))),NumberedField(2,RequiredField(Field(BaseField(BaseType.I32),Identifier(\"num2\"),None))),NumberedField(3,RequiredField(Field(IdentifierField(Identifier(\"Operation\")),Identifier(\"op\"),None))),NumberedField(4,RequiredField(Field(BaseField(BaseType.String),Identifier(\"comment\"),None)))])))"
            // Don't think we need to iterate over all the struct options

        module ``union`` = 
            [<Test>]
            let ``Can parse a Union`` () = 
                let testdata = """union WorkUnion {
                    1: i32 intVal
                    2: string stringVal
                    3: Operation operationVal }"""
                match testdata with
                | Union (union,_) ->
                    union |> should equal (Ast.UnionDefinition(Ast.Union(Ast.Identifier("WorkUnion"),
                                                                         [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("intVal"),None)))
                                                                          Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.String),Ast.Identifier("stringVal"),None)))
                                                                          Ast.NumberedField(3,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("Operation")),Ast.Identifier("operationVal"),None)))
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
                match testdata with
                | Service (service,_) -> 
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None)))
                                                                                              Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],None)))"
            [<Test>]
            let ``Can parse a service with inheritance and no exceptions`` () = 
                let testdata = """service MyService extends AnotherService {
                                      void ping(),
                                      i32 add(1:i32 num1;2:i32 num2)
                                  }
                               """
                match testdata with
                | Service (service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None)))
                                                                                              Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                                             ],[])],Some <| Ast.Identifier("AnotherService"))))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],Some (Identifier(\"AnotherService\"))))"
            [<Test>]
            let ``Can parse a service with exceptions`` () = 
                let testdata = """service MyService {
                                      void ping() throws (1:SomeException err1,2:AnotherException err2),
                                      i32 add(1:i32 num1;2:i32 num2)
                                  }
                               """
                match testdata with
                | Service (service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("SomeException")),Ast.Identifier("err1"),None)));
                                                                                                                            Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.IdentifierField(Ast.Identifier("AnotherException")),Ast.Identifier("err2"),None)))]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None)))
                                                                                              Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[NumberedField(1,RequiredField(Field(IdentifierField(Identifier(\"SomeException\")),Identifier(\"err1\"),None)));NumberedField(2,RequiredField(Field(IdentifierField(Identifier(\"AnotherException\")),Identifier(\"err2\"),None)))]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],None)))"
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
                match testdata with
                | Service(service,_) ->
                    service |> should equal (Ast.ServiceDefinition(Ast.Service(Ast.Identifier("MyService"),
                                                                               [Ast.VoidFunction(Ast.Identifier("ping"),[],[]);
                                                                                Ast.Function(Ast.BaseField(Ast.I32),
                                                                                             Ast.Identifier("add"),
                                                                                             [Ast.NumberedField(1,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num1"),None)))
                                                                                              Ast.NumberedField(2,Ast.RequiredField(Ast.Field(Ast.BaseField(Ast.I32),Ast.Identifier("num2"),None)))
                                                                                             ],[])],None)))
                | _ -> failwith "Expecting ServiceDefinition(Service(Identifier(\"MyService\"),[VoidFunction(Identifier(\"ping\"),[],[]);Function(BaseField(I32),Identifier(\"add\"),[NumberedField(1,RequiredField(Field(BaseField(I32),Identifier(\"num1\"),None)));NumberedField(2,RequiredField(Field(BaseField(I32),Identifier(\"num2\"),None)))],[])],Some (Identifier(\"AnotherService\"))))"