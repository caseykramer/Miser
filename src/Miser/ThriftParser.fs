module ThriftParser

open FParsec
open CharParsers

let delimitedBy c p = pstring c >>. p .>> pstring c

let zeroOrOne = opt
let zeroOrMore = many
let oneOrMore = many1
let digit:Parser<char,unit> = anyOf ['0'..'9']
let digits:Parser<string,unit> = many digit >>= (List.toArray >> (fun c -> new System.String(c)) >> pstring)
let letter:Parser<char,unit> = anyOf ['A'..'Z'] <|> anyOf ['a'..'z']
let letters:Parser<string,unit> = many letter >>= (List.toArray >> (fun c -> new System.String(c)) >> pstring)
let letterOrDigit = letter <|> digit
let lettersOrDigits = many letterOrDigit >>= (List.toArray >> (fun c -> new System.String(c)) >> pstring)
let charToString c = pstring (string c)
let charsToString cs = pstring (System.String(List.toArray cs))

let listSeparator:Parser<char,unit> = anyOf [';';',']

let singleLineComment:Parser<string,unit> = pstring "//" >>. restOfLine true
let multiLineComment:Parser<string,unit> = pstring "/*" >>. anyString (System.Int32.MaxValue) .>> pstring "/*"

let ws = spaces <|> (singleLineComment >>. preturn ()) <|> (multiLineComment >>. (preturn ()))

// Identifiers
let identifier = (letter >>= charToString) <|> pstring "_" >>. 
                    (oneOrMore (letter <|> 
                                digit <|> 
                                pchar '.' <|> 
                                pchar '_' <|> 
                                pchar '-') >>= charsToString) .>> ws |>> ThriftAST.Identifier

// Literals
let literal:Parser<ThriftAST.Literal,unit> = 
    let doubleDelimited = (delimitedBy "\"" ((many (noneOf ['"'])) >>= charsToString)) 
    let singleDelimited = (delimitedBy "'" ((many (noneOf ['\''])) >>= charsToString))
    (doubleDelimited <|> singleDelimited) .>> ws |>> ThriftAST.StringLiteral

// Constants
let intConstant:Parser<ThriftAST.Constant,unit> = pint64 .>> ws |>> ThriftAST.IntegerConstant
let doubleConstant:Parser<ThriftAST.Constant,unit> = pfloat .>> ws |>> ThriftAST.DecimalConstant
let literalConstant = literal |>> ThriftAST.LiteralConstant
let identifierConstant = identifier |>> ThriftAST.IdentifierConstant
let constantValue,constantValueRef = createParserForwardedToRef<ThriftAST.Constant,unit>()
let listConstant = (pstring "[" >>. (many (constantValue .>> opt listSeparator) .>> ws) .>> pstring "]") .>> ws |>> ThriftAST.ListConstant
let mapConstant = 
    let keyValue = (constantValue .>> pstring ":" .>>. constantValue .>> opt listSeparator) .>> ws
    (pstring "{" >>. (many keyValue) .>> pstring "}") .>> ws |>> ThriftAST.MapConstant
do constantValueRef := intConstant <|> doubleConstant <|> literalConstant <|> identifierConstant <|> listConstant <|> mapConstant

// Types
let baseType:Parser<ThriftAST.BaseType,unit> = 
    (pstring "bool" >>. ws >>. preturn ThriftAST.BaseType.Bool) <|>
    (pstring "byte" >>. ws >>. preturn ThriftAST.BaseType.Byte) <|>
    (pstring "i16" >>. ws >>. preturn ThriftAST.BaseType.I16) <|>
    (pstring "i32" >>. ws >>. preturn ThriftAST.BaseType.I32) <|>
    (pstring "i64" >>. ws >>. preturn ThriftAST.BaseType.I64) <|>
    (pstring "double" >>. ws >>. preturn ThriftAST.BaseType.Double) <|>
    (pstring "string" >>. ws >>. preturn ThriftAST.BaseType.String) <|>
    (pstring "binary" >>. ws >>. preturn ThriftAST.BaseType.Binary) <|>
    (pstring "slist" >>. ws >>. preturn ThriftAST.BaseType.SList)

let fieldType,fieldTypeRef = createParserForwardedToRef<ThriftAST.FieldType,unit>()
let mapType = pstring "map" >>. pstring "<" >>. fieldType .>> pstring "," .>> ws .>>. fieldType .>> pstring ">" .>> ws |>> ThriftAST.ContainerType.Map
let setType = pstring "set" >>. pstring "<" >>. fieldType .>> pstring ">" .>> ws |>> ThriftAST.ContainerType.Set
let listType = pstring "list" >>. pstring "<" >>. fieldType .>> pstring ">" .>> ws |>> ThriftAST.ContainerType.List
let containerType = (mapType <|> setType <|> listType) .>> ws
let definitionType = ((baseType |>> ThriftAST.BaseDefinition) <|> (containerType |>> ThriftAST.ContainerDefinition) .>> ws)
do fieldTypeRef := (identifier |>> ThriftAST.IdentifierField) <|> (containerType |>> ThriftAST.ContainerField) <|> (baseType |>> ThriftAST.BaseField) .>> ws


// Fields
let fieldId = (intConstant >>= (function | ThriftAST.IntegerConstant v -> preturn v | _ -> fail "expected integer constant")) .>> pstring ":" .>> ws
let requiredness = 
    (pstring "required" .>> ws >>. preturn true) <|> 
    (pstring "optional" .>> ws >>. preturn false)
let field = 
    (opt fieldId) .>>. (opt requiredness) .>>. fieldType .>>. identifier .>>. opt(pstring "=" >>. ws >>. constantValue) .>> ws .>> opt (listSeparator) .>> ws
        |>> (fun ((((id,req),t),i),v) -> ThriftAST.Field (id,req,t,i,v))

// Functions
let throws = pstring "throws" >>.ws >>. pstring "(" >>. ws >>. many field .>> ws .>> pstring ")" .>> ws |>> ThriftAST.Throws
let functionType = 
    (fieldType |>> ThriftAST.FunctionType.Type) <|> 
    (pstring "void" >>. preturn ThriftAST.FunctionType.Void) .>> ws
let functionDef = 
    let isOneway = opt ((pstring "oneway") >>. preturn true) .>> ws
    isOneway .>>. functionType .>>. identifier .>>. (pstring "(" >>. ws >>. many field .>> ws .>> pstring ")" .>> ws) .>>. opt throws .>> opt listSeparator .>> ws
        |>> (fun ((((o,t),i),f),tr) ->  ThriftAST.Function (o,t,i,f,tr))

// Service
let serviceDef = 
    let servicedecl = pstring "service" >>. ws >>. identifier .>>. (opt (pstring "extends" >>. ws >>. identifier)) 
    servicedecl .>>. (pstring "{" >>. ws >>. many functionDef .>> ws .>> pstring "}" .>> ws) 
        |>> (fun ((n,e),f) -> ThriftAST.Service (n,e,f))

// Exception
let exceptionDef =pstring "exception" >>. ws >>. identifier .>>. (pstring "{" >>. ws >>. many field .>> ws .>> pstring "}" .>> ws) |>> ThriftAST.Exception

// Union
let unionDef = pstring "union" >>. ws >>. identifier .>>. (pstring "{" >>. ws >>. many field .>> ws .>> pstring "}" .>> ws) |>> ThriftAST.Union

// Struct
let structDef = pstring "struct" >>. ws >>. identifier .>>. (pstring "{" >>. ws >>. many field .>> ws .>> pstring "}" .>> ws) |>> ThriftAST.Struct

// Enum
let enumDef = 
    let valueDef = identifier .>>. (opt pint64 .>> ws) .>> opt listSeparator .>> ws
    pstring "enum" >>. ws >>. identifier .>>. (pstring "{" >>. ws >>. many valueDef .>> ws .>> pstring "}" .>> ws) |>> ThriftAST.Enum

// Typedef
let typeDef = pstring "typedef" >>. ws >>. definitionType .>>. identifier |>> ThriftAST.TypeDef

// Const
let constDef = 
    pstring "const" >>. ws >>. fieldType .>>. identifier .>>. (pstring "=" >>. ws >>. constantValue) .>> opt listSeparator .>> ws
        |>> (fun ((t,i),v) -> ThriftAST.Const (t,i,v))

// Definition
let definitionInfo = 
    (constDef |>> ThriftAST.ConstDef) <|>
    (typeDef |>> ThriftAST.DefinitionInfo.TypeDef) <|>
    (enumDef |>> ThriftAST.EnumDef) <|>
    (structDef |>> ThriftAST.StructDef) <|>
    (unionDef |>> ThriftAST.UnionDef) <|>
    (exceptionDef |>> ThriftAST.ExceptionDef) <|>
    (serviceDef |>> ThriftAST.ServiceDef) .>> ws

// Namespace
let namespaceScope:Parser<ThriftAST.NamespaceScope,unit> = 
    (pstring "*" >>. ws >>. preturn ThriftAST.NamespaceScope.All) <|>
    (pstring "cpp" >>. ws >>. preturn ThriftAST.NamespaceScope.Cpp) <|>
    (pstring "java" >>. ws >>. preturn ThriftAST.NamespaceScope.Java) <|>
    (pstring "py" >>. ws >>. preturn ThriftAST.NamespaceScope.Py) <|>
    (pstring "perl" >>. ws >>. preturn ThriftAST.NamespaceScope.Perl) <|>
    (pstring "rb" >>. ws >>. preturn ThriftAST.NamespaceScope.Ruby) <|>
    (pstring "cocoa" >>. ws >>. preturn ThriftAST.NamespaceScope.Cocoa) <|>
    (pstring "csharp" >>. ws >>. preturn ThriftAST.NamespaceScope.CSharp) <|>
    (pstring "fsharp" >>. ws >>. preturn ThriftAST.NamespaceScope.FSharp) <|>
    (pstring "php" >>. ws >>. preturn ThriftAST.NamespaceScope.Php) <|>
    (anyString System.Int32.MaxValue .>> ws |>> ThriftAST.NamespaceScope.Other)
let namespaceDef = pstring "namespace" >>. ws >>. namespaceScope .>>. identifier |>> ThriftAST.Namespace


// Include
let includeDef = (pstring "include" >>. ws >>. literal |>> ThriftAST.Include) <|> (pstring "cpp_include" >>. ws >>. literal |>> ThriftAST.Include)

// Header
let headerInfo = (includeDef |>> ThriftAST.IncludeHeader) <|> (namespaceDef |>> ThriftAST.NamespaceHeader)

// Document
let document = (many headerInfo) .>>. (many definitionInfo) |>> ThriftAST.Document