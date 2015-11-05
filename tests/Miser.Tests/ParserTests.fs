module ``Parser Tests``
   
open FParsec
open NUnit.Framework
open FsUnit

let shouldParseWith (parser:Parser<'a,unit>) (text:string) = 
    match run parser text with
    | Success(v:'a,_,_) -> v
    | Failure(err,_,_) -> 
        Assert.Fail(err)
        Unchecked.defaultof<'a>

let parseFail expected got = 
    Assert.Fail(sprintf "invalid result. expected '%s' but got '%A'" expected got)
    
module Basics = 
    module Comments = 
        [<Test>]
        let ``Can parse a single-line comment``() = 
            let comment = """// This is a comment"""
            match comment |> shouldParseWith ThriftParser.singleLineComment with
            | s -> s |> should equal " This is a comment"

        [<Test>]
        let ``Can parse a multi-line comment``() = 
            let comment = """/**
 * This is a comment
 *
 */"""
            match comment |> shouldParseWith ThriftParser.multilineComment with
            | s ->
                let expected = """*
 * This is a comment
 *
 """
                let expected = expected.Replace("\r\n","\n")
                s |> should equal expected

    module Identifier = 
        
        [<Test>]
        let ``Can parse an identifier starting with a lower-case letter``() =
            let identifierStr = "someIdentifier   "
            match identifierStr |> shouldParseWith ThriftParser.identifier with
            | ThriftAST.Identifier id -> id |> should equal "someIdentifier"
            
        [<Test>]
        let ``Can parse an identifier starting with an upper-case letter``() = 
            let identifierStr = "SomeIdentifier"
            match identifierStr |> shouldParseWith ThriftParser.identifier with
            | ThriftAST.Identifier id -> id |> should equal "SomeIdentifier"
            
        [<Test>]
        let ``Can parse an identifier starting with an underscore``() = 
            let identifierStr = "_someIdentifier"
            match identifierStr |> shouldParseWith ThriftParser.identifier with
            | ThriftAST.Identifier(id) -> id |> should equal "_someIdentifier"
            
        [<Test>]
        let ``Can parse an identifier ending with a comment``() = 
            let identifierStr = "_someIdentifier // This is an identifier"
            let result = run ThriftParser.identifier 
            match identifierStr |> shouldParseWith ThriftParser.identifier with
            | ThriftAST.Identifier(id) -> id |> should equal "_someIdentifier"
            
    module Constant = 
        
        [<Test>]
        let ``Can parse an integer constant``() = 
            let constant = "1234 "
            match constant |> shouldParseWith ThriftParser.constantValue with
            | ThriftAST.Constant.IntegerConstant(v) -> v |> should equal 1234
            | v -> parseFail "Constant.IntegerConstant(v)" v

        [<Test>]
        let ``Can pare a decimal constant``() = 
            let constant = "1.0"
            match constant |> shouldParseWith ThriftParser.constantValue with
            | ThriftAST.Constant.DecimalConstant(v) -> v |> should equal 1.0
            | v -> parseFail "Constant.DecimalConstant(v)" v

module Header = 

    module Include = 
        
        [<Test>]
        let ``Can parse a basic include``() = 
            let includeStrQuote = """include "somefile.thrift" """
            let includeStrSingleQuote = """include 'somefile.thrift'"""
            match includeStrQuote |> shouldParseWith ThriftParser.includeDef with
            | ThriftAST.Include (ThriftAST.StringLiteral s) -> s |> should equal "somefile.thrift"
            
            let result = run ThriftParser.includeDef includeStrSingleQuote
            match includeStrSingleQuote |> shouldParseWith ThriftParser.includeDef with
            | ThriftAST.Include (ThriftAST.StringLiteral s) -> s |> should equal "somefile.thrift"
            
    module Namespace = 
        
        [<Test>]
        let ``Can parse "All" namespace declaration``() = 
            let namespaceStr = "namespace * test.namespace"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.All,None,ThriftAST.Identifier(ns)) -> ns |> should equal "test.namespace"
            | n -> Assert.Fail(sprintf "invalid result. expected Namesapce(NamespaceScope.All,Identifier(ns))' but got '%A'" n)

        [<Test>]
        let ``Can parse "Java" namespace declaration``() = 
            let namespaceStr = "namespace java com.test.namespace"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.Java,None,ThriftAST.Identifier(ns)) -> ns |> should equal "com.test.namespace"
            | n -> parseFail "Namesapce(NamespaceScope.Java,Identifier ns)" n

        [<Test>]
        let ``Can parse "Cpp" namespace declaration``() = 
            let namespaceStr = "namespace cpp test.namespace"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.Cpp,None,ThriftAST.Identifier (ns)) -> ns |> should equal "test.namespace"
            | n -> parseFail "Namespace(NamespaceScope.Cpp,Identifier ns)" n

        [<Test>]
        let ``Can parse "CSharp" namespace declaration``() = 
            let namespaceStr = "namespace csharp Test.Namespace"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.CSharp,None,ThriftAST.Identifier(ns)) -> ns |> should equal "Test.Namespace"
            | n -> parseFail "Namespace(NamespaceScope.CSharp,Identifier ns)" n

        [<Test>]
        let ``Can parse a namespace with an unknown scope``() = 
            let namespaceStr = "namespace something Some.Namespace"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.Other(scope),None,ThriftAST.Identifier(ns)) ->
                scope |> should equal "something"
                ns |> should equal "Some.Namespace"
            | n -> parseFail "Namespace(NamespaceScope.Other(s),Identifier(ns)" n

        [<Test>]
        let ``Can parse a namespace with a multi-part scope``() = 
            let namespaceStr = "namespace fsharp.module MyModule"
            match namespaceStr |> shouldParseWith ThriftParser.namespaceDef with
            | ThriftAST.Namespace(ThriftAST.NamespaceScope.FSharp,Some(sub),ThriftAST.Identifier(ns)) ->
                sub |> should equal "module"
                ns |> should equal "MyModule"
            | n -> parseFail "Namespace(NamespaceScope.FSharp,Some(sub)Identifier(ns))" n

    [<Test>]
    let ``Can parse multiple namespace references``() = 
        let testHeader = """namespace * test.namesace
namespace cpp test.namesace
namespace csharp Test.Namesace
"""
        match testHeader |> shouldParseWith ThriftParser.document with
        | ThriftAST.Document(headers,body) ->
            match headers with
            | [ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.All,None,ThriftAST.Identifier(n1)))
               ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.Cpp,None,ThriftAST.Identifier(n2)))
               ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.CSharp,None,ThriftAST.Identifier(n3)))
              ] ->
                n1 |> should equal "test.namesace"
                n2 |> should equal "test.namesace"
                n3 |> should equal "Test.Namesace"
            | _ -> Assert.Fail(sprintf "headers incorrect. %A" headers)
        
    [<Test>]
    let ``Can parse multiple namespace references with a starting comment``() = 
        let testHeader = """/* This is some kind of comment block
 *
 * It spans multiple lines
 */

namespace * test.namesace
namespace cpp test.namesace
namespace csharp Test.Namesace
"""
        match testHeader |> shouldParseWith (ThriftParser.anyComment >>. many (ThriftParser.headerInfo)) with
        | [ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.All,None,ThriftAST.Identifier(n1)))
           ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.Cpp,None,ThriftAST.Identifier(n2)))
           ThriftAST.NamespaceHeader(ThriftAST.Namespace(ThriftAST.NamespaceScope.CSharp,None,ThriftAST.Identifier(n3)))
          ] ->
                n1 |> should equal "test.namesace"
                n2 |> should equal "test.namesace"
                n3 |> should equal "Test.Namesace"
            | n -> Assert.Fail(sprintf "headers incorrect. %A" n)
                   
module Definition = 

    module TypeDef = 
        [<Test>]
        let ``Can Parse a type definition for a boolean type``() = 
            let typedef = "typedef bool MyBool"
            match typedef |> shouldParseWith ThriftParser.typeDef with
            | ThriftAST.TypeDef(ThriftAST.DefinitionType.BaseDefinition(ThriftAST.BaseType.Bool),ThriftAST.Identifier(n)) -> n |> should equal "MyBool"
            | n -> parseFail "TypeDef(DestinationType.BaseDefinition(BaseType.Bool),Identifier(n))" n

        [<Test>]
        let ``Can Parse a type definition for a byte type``() = 
            let typedef = "typedef byte MyByte"
            match typedef |> shouldParseWith ThriftParser.typeDef with
            | ThriftAST.TypeDef(ThriftAST.DefinitionType.BaseDefinition(ThriftAST.BaseType.Byte),ThriftAST.Identifier(n)) -> n |> should equal "MyByte"
            | n -> parseFail "TypeDef(DestinationType.BaseDefinition(BaseType.Byte),Identifier(n))" n

    module Const = 
        [<Test>]
        let ``Can parse a Const declaration``() = 
            let constDef = "const i16 MyValue = 1"
            match constDef |> shouldParseWith ThriftParser.constDef with
            | ThriftAST.Const(ThriftAST.FieldType.BaseField(ThriftAST.BaseType.I16),ThriftAST.Identifier(n),ThriftAST.IntegerConstant(v)) ->
                v |> should equal 1L
            | n -> parseFail "Const(FieldType.BaseField(BaseType.I16),Identifier(n),IntegerConstant(v))" n

    module Enum =
        [<Test>]
        let ``Can parse an enum definition``() = 
            let enumDef = """enum MyEnum {
                ONE = 1,
                TWO = 2,
                THREE = 3
            }"""
            match enumDef |> shouldParseWith ThriftParser.enumDef with
            | ThriftAST.Enum(ThriftAST.Identifier(enumName),values) ->
                enumName |> should equal "MyEnum"
                match values with
                | [ThriftAST.Identifier(i1),Some v1;ThriftAST.Identifier(i2),Some v2;ThriftAST.Identifier(i3),Some v3] ->
                    i1 |> should equal "ONE"
                    v1 |> should equal 1L
                    i2 |> should equal "TWO"
                    v2 |> should equal 2L
                    i3 |> should equal "THREE"
                    v3 |> should equal 3L
                | v -> Assert.Fail(sprintf "Values are not correct: %A" v)

    module Struct = 
        [<Test>]
        let ``Can parse a struct definition``() = 
            let structDef = """struct MyStruct {
 1: string test1,
 2: i32 test2
 }"""
            match structDef |> shouldParseWith ThriftParser.structDef with
            | ThriftAST.Struct(ThriftAST.Identifier(name),fields) ->
                name |> should equal "MyStruct"
                match fields with
                | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name1),None);
                   ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.I32),ThriftAST.Identifier(name2),None)] ->
                    id1 |> should equal 1L
                    name1 |> should equal "test1"
                    id2 |> should equal 2L
                    name2 |> should equal "test2"
                | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)

    module Union = 
        [<Test>]
        let ``Can parse a union definition``() = 
            let unionDef = """union MyUnion {
 1: string test1,
 2: i32 test2
 }"""
            match unionDef |> shouldParseWith ThriftParser.unionDef with
            | ThriftAST.Union(ThriftAST.Identifier(name),fields) ->
                name |> should equal "MyUnion"
                match fields with
                | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name1),None);
                   ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.I32),ThriftAST.Identifier(name2),None)] ->
                    id1 |> should equal 1L
                    name1 |> should equal "test1"
                    id2 |> should equal 2L
                    name2 |> should equal "test2"
                | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)

    module Exception = 
        [<Test>]
        let ``Can parse an exception definition``() = 
            let excpDef = """exception MyError {
1: string test1,
2: i32 test2
}"""
            match excpDef |> shouldParseWith ThriftParser.exceptionDef with
            | ThriftAST.Exception(ThriftAST.Identifier(name),fields) ->
                name |> should equal "MyError"
                match fields with
                | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name1),None);
                   ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.I32),ThriftAST.Identifier(name2),None)] ->
                    id1 |> should equal 1L
                    name1 |> should equal "test1"
                    id2 |> should equal 2L
                    name2 |> should equal "test2"
                | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)

    module Service = 
        
        module Function = 
            [<Test>]
            let ``Can parse a function definition``() = 
                let functionDef = """i32 MyFunction(1:bool isReal,2:string name)"""
                match functionDef |> shouldParseWith ThriftParser.functionDef with
                | ThriftAST.Function(None,ThriftAST.FunctionType.Type(ThriftAST.BaseField(ThriftAST.BaseType.I32)),ThriftAST.Identifier(name),fields,None) ->
                    name |> should equal "MyFunction"
                    match fields with
                    | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.Bool),ThriftAST.Identifier(name1),None);
                       ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name2),None) ] ->
                       id1 |> should equal 1L
                       name1 |> should equal "isReal"
                       id2 |> should equal 2L
                       name2 |> should equal "name"
                    | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)
                | v -> parseFail "Function(None,Type(BaseField(BaseType.I32)),Identifier(nme),fields,None)" v

            [<Test>]
            let ``Can parse a void/oneway function``() = 
                let functiondef = """oneway void MyVoidFunction(1:bool isReal,2:string name)"""
                match functiondef |> shouldParseWith ThriftParser.functionDef with
                | ThriftAST.Function(Some oneWay,ThriftAST.FunctionType.Void,ThriftAST.Identifier(name),fields,None) ->
                    name |> should equal "MyVoidFunction"
                    oneWay |> should be True
                    match fields with
                    | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.Bool),ThriftAST.Identifier(name1),None);
                       ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name2),None) ] ->
                       id1 |> should equal 1L
                       name1 |> should equal "isReal"
                       id2 |> should equal 2L
                       name2 |> should equal "name" 
                    | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)
                | v -> parseFail "Function(Some oneWay,FunctionType.Void,Identifier(name),fields,None)" v

            [<Test>]
            let ``Can parse a function which throws an exception``() = 
                let functiondef = """i32 MyFunction(1:bool isReal,2:string name) throws (1: ErrorType error)"""
                match functiondef |> shouldParseWith ThriftParser.functionDef with
                | ThriftAST.Function(None,ThriftAST.FunctionType.Type(ThriftAST.BaseField(ThriftAST.BaseType.I32)),ThriftAST.Identifier(name),fields,Some (ThriftAST.Throws(throws))) ->
                    name |> should equal "MyFunction"
                    match fields with
                    | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.Bool),ThriftAST.Identifier(name1),None);
                       ThriftAST.Field(Some(id2),None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(name2),None) ] ->
                       id1 |> should equal 1L
                       name1 |> should equal "isReal"
                       id2 |> should equal 2L
                       name2 |> should equal "name" 
                    | v -> Assert.Fail(sprintf "Fields are not as expected: %A" v)
                    match throws with
                    | [ThriftAST.Field(Some(id1),None,ThriftAST.FieldType.IdentifierField(ThriftAST.Identifier(fldType)),ThriftAST.Identifier(name1),None)] ->
                        id1 |> should equal 1L
                        fldType |> should equal "ErrorType"
                        name1 |> should equal "error"
                    | v -> Assert.Fail(sprintf "Error fields are not as expected: %A" v)
                | v -> parseFail "Function(None,FunctionType.Type(BaseField(BaseType.I32)),Identifier(name),fields,Some (Throws(throws))" v

            [<Test>]
            let ``Can parse a function definition with a comment``() = 
                let functiondef = """ /**
   * Prints "testVoid()" and returns nothing.
   */
  void         testVoid()"""
                match functiondef |> shouldParseWith ThriftParser.functionDef with
                | ThriftAST.Function(None,ThriftAST.FunctionType.Void,ThriftAST.Identifier(fname),[],None) ->
                    fname |> should equal "testVoid"
                | v -> parseFail "Function(None,FunctionType.Void,Identifier(fname),[],None)" v

            [<Test>]
            let ``Can parse multiple function definitions with comments``() = 
                let functiondef = """ /**
   * Prints "testVoid()" and returns nothing.
   */
  void         testVoid(),

  /**
   * Prints 'testString("%s")' with thing as '%s'
   * @param string thing - the string to print
   * @return string - returns the string 'thing'
   */
  string       testString(1: string thing),
"""
                match functiondef |> shouldParseWith (many ThriftParser.functionDef) with
                | [f1;f2] ->
                    match f1 with
                    | ThriftAST.Function(None,ThriftAST.FunctionType.Void,ThriftAST.Identifier(f1Name),[],None) ->
                        f1Name |> should equal "testVoid"
                    | v -> parseFail "Function(None,Void,Identifier(name),[],None)" v
                    match f2 with
                    | ThriftAST.Function(None,ThriftAST.FunctionType.Type(ThriftAST.BaseField(ThriftAST.BaseType.String)),ThriftAST.Identifier(f2Name),[p1],None) ->
                        f2Name |> should equal "testString"
                        match p1 with
                        | ThriftAST.FieldDefinition.Field(Some fId,None,ThriftAST.FieldType.BaseField(ThriftAST.BaseType.String),ThriftAST.Identifier(fname),None) ->
                            fId |> should equal 1L
                            fname |> should equal "thing"
                        | v -> parseFail "Field(Some fid,None,BaseField(BaseType.String),Identifier(name),None)" v
                    | v -> parseFail "Function(None,FunctionType.Type(BaseField(BaseType.String)),Identifier(name),[p1],None)" v
                | v -> parseFail "[f1;f2]" v

            [<Test>]
            let ``Can parse a large list of function definitions``() = 
                let functiondef = """  /**
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
   * Prints 'testBool("%s")' where '%s' with thing as 'true' or 'false'
   * @param bool  thing - the bool data to print
   * @return bool  - returns the bool 'thing'
   */
  bool         testBool(1: bool thing),

  /**
   * Prints 'testByte("%d")' with thing as '%d'
   * The types i8 and byte are synonyms, use of i8 is encouraged, byte still exists for the sake of compatibility.
   * @param byte thing - the i8/byte to print
   * @return i8 - returns the i8/byte 'thing'
   */
  i8         testByte(1: byte thing),

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
   * Prints 'testBinary("%s")' where '%s' is a hex-formatted string of thing's data
   * @param binary  thing - the binary data to print
   * @return binary  - returns the binary 'thing'
   */
  binary       testBinary(1: binary thing),
  
  /**
   * Prints 'testStruct("{%s}")' where thing has been formatted into a string of comma separated values
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
   *  separated by commas and new lines
   * @param map<i32,i32> thing - the map<i32,i32> to print
   * @return map<i32,i32> - returns the map<i32,i32> 'thing'
   */
  map<i32,i32> testMap(1: map<i32,i32> thing),

  /**
   * Prints 'testStringMap("{%s}")' where thing has been formatted into a string of  'key => value' pairs
   *  separated by commas and new lines
   * @param map<string,string> thing - the map<string,string> to print
   * @return map<string,string> - returns the map<string,string> 'thing'
   */
  map<string,string> testStringMap(1: map<string,string> thing),

  /**
   * Prints 'testSet("{%s}")' where thing has been formatted into a string of  values
   *  separated by commas and new lines
   * @param set<i32> thing - the set<i32> to print
   * @return set<i32> - returns the set<i32> 'thing'
   */
  set<i32>     testSet(1: set<i32> thing),

  /**
   * Prints 'testList("{%s}")' where thing has been formatted into a string of  values
   *  separated by commas and new lines
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
   * elsen if arg0 == "Xception2" throw Xception2 with errorCode = 2002 and struct_thing.string_thing = "This is an Xception2"
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
"""
                match functiondef |> shouldParseWith (many ThriftParser.functionDef) with
                | [] -> Assert.Fail("No functions parsed")
                | items -> items |> List.length |> should equal 22
        [<Test>]
        let ``Can pase simple service definition``() = 
            let svc = """service MyService {
    void testVoid()
}"""
            match svc |> shouldParseWith ThriftParser.serviceDef with
            | ThriftAST.Service(ThriftAST.Identifier(svcName),None,functions) ->
                svcName |> should equal "MyService"
                functions |> List.length |> should equal 1
                let function1 = functions |> List.head
                match function1 with
                | ThriftAST.Function(None,ThriftAST.Void,ThriftAST.Identifier(name),[],None) ->
                    name |> should equal "testVoid"
                | v -> parseFail "Function(None,Void,Identifer(name),[],None)" v
            | v -> parseFail "Service(Identifier(svcName),None,functions)" v

        [<Test>]
        let ``Can parse a serice definition which extends another service``() = 
            let svc = """service MyService extends AnotherService {
    void testVoid()
}"""
            match svc |> shouldParseWith ThriftParser.serviceDef with
            | ThriftAST.Service(ThriftAST.Identifier(svcName),Some (ThriftAST.Identifier(extends)),functions) ->
                svcName |> should equal "MyService"
                extends |> should equal "AnotherService"
                functions |> List.length |> should equal 1
                let function1 = functions |> List.head
                match function1 with
                | ThriftAST.Function(None,ThriftAST.Void,ThriftAST.Identifier(name),[],None) ->
                    name |> should equal "testVoid"
                | v -> parseFail "Function(None,Void,Identifer(name),[],None)" v
            | v -> parseFail "Service(Identifier(svcName),Some (Identifier(extends)),functions)" v

        [<Test>]
        let ``Can parse large service definition with doc-comments``() = 
            let svc = """service ThriftTest
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
   * Prints 'testBool("%s")' where '%s' with thing as 'true' or 'false'
   * @param bool  thing - the bool data to print
   * @return bool  - returns the bool 'thing'
   */
  bool         testBool(1: bool thing),

  /**
   * Prints 'testByte("%d")' with thing as '%d'
   * The types i8 and byte are synonyms, use of i8 is encouraged, byte still exists for the sake of compatibility.
   * @param byte thing - the i8/byte to print
   * @return i8 - returns the i8/byte 'thing'
   */
  i8         testByte(1: byte thing),

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
   * Prints 'testBinary("%s")' where '%s' is a hex-formatted string of thing's data
   * @param binary  thing - the binary data to print
   * @return binary  - returns the binary 'thing'
   */
  binary       testBinary(1: binary thing),
  
  /**
   * Prints 'testStruct("{%s}")' where thing has been formatted into a string of comma separated values
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
   *  separated by commas and new lines
   * @param map<i32,i32> thing - the map<i32,i32> to print
   * @return map<i32,i32> - returns the map<i32,i32> 'thing'
   */
  map<i32,i32> testMap(1: map<i32,i32> thing),

  /**
   * Prints 'testStringMap("{%s}")' where thing has been formatted into a string of  'key => value' pairs
   *  separated by commas and new lines
   * @param map<string,string> thing - the map<string,string> to print
   * @return map<string,string> - returns the map<string,string> 'thing'
   */
  map<string,string> testStringMap(1: map<string,string> thing),

  /**
   * Prints 'testSet("{%s}")' where thing has been formatted into a string of  values
   *  separated by commas and new lines
   * @param set<i32> thing - the set<i32> to print
   * @return set<i32> - returns the set<i32> 'thing'
   */
  set<i32>     testSet(1: set<i32> thing),

  /**
   * Prints 'testList("{%s}")' where thing has been formatted into a string of  values
   *  separated by commas and new lines
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
   * elsen if arg0 == "Xception2" throw Xception2 with errorCode = 2002 and struct_thing.string_thing = "This is an Xception2"
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
}"""
            match svc |> shouldParseWith ThriftParser.serviceDef with
            | ThriftAST.Service(ThriftAST.Identifier(svcName),None,functions) ->
                svcName |> should equal "ThriftTest"
                functions |> List.length |> should equal 22
            | v -> parseFail "Service(Identifier(svcName),None,functions)" v