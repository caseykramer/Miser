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

let singleLineComment:Parser<string,unit> = (pstring "//" <|> pstring "#") >>. restOfLine true
let rec docComment =
    let openDocCommentStr = "/**"
    let closeDocCommentStr = "*/"
    let ign x = charsTillString x false System.Int32.MaxValue
    between (pstring openDocCommentStr)
            (pstring closeDocCommentStr)
            (ign closeDocCommentStr)
let rec multilineComment =
    let openMultilineCommentStr = "/*"
    let closeMultilineCommentStr = "*/"
    let ign x = charsTillString x false System.Int32.MaxValue
    between
        (pstring openMultilineCommentStr)
        (pstring closeMultilineCommentStr)
        (ign closeMultilineCommentStr)

let anyComment = docComment <|> (multilineComment) <|> (many singleLineComment |>> (function | [] -> "" | l -> l |> List.reduce (sprintf "%s\n%s"))) .>> opt spaces

let ws = skipSepBy spaces (singleLineComment <|> multilineComment) 
let (<||>) p1 p2 = fun x -> p1 x || p2 x

// Identifiers
let identifier = 
    let isStart = isAsciiLetter <||> ((=) '_')
    let isContinue = isAsciiLetter <||> isDigit <||> ((=) '_') <||> ((=) '-') <||> ((=) '.')
    CharParsers.identifier (IdentifierOptions(isStart,isContinue)) |>> ThriftAST.Identifier

// Literals
let literal:Parser<ThriftAST.Literal,unit> = 
    let doubleDelimited = between (pstring "\"") (pstring "\"") (manySatisfy ((<>) '"'))
    let singleDelimited = between (pstring "'") (pstring "'") (manySatisfy ((<>) '\''))
    (doubleDelimited <|> singleDelimited) .>> ws |>> ThriftAST.StringLiteral

// Constants
let intConstant:Parser<ThriftAST.Constant,unit> = pint64 |>> ThriftAST.IntegerConstant .>> ws
let doubleConstant:Parser<ThriftAST.Constant,unit> = 
    let parse s = 
        match System.Double.TryParse(s) with
        | true,v -> ThriftAST.DecimalConstant v
        | _ -> ThriftAST.IntegerConstant (System.Int64.Parse(s)) 
    numberLiteral (NumberLiteralOptions.AllowMinusSign ||| 
                   NumberLiteralOptions.AllowPlusSign ||| 
                   NumberLiteralOptions.AllowFractionWOIntegerPart ||| 
                   NumberLiteralOptions.AllowFraction) "decimal" |>> fun nl -> if nl.HasFraction then parse nl.String else ThriftAST.IntegerConstant (System.Int64.Parse(nl.String))
let literalConstant = literal |>> ThriftAST.LiteralConstant
let identifierConstant = identifier |>> ThriftAST.IdentifierConstant
let constantValue,constantValueRef = createParserForwardedToRef<ThriftAST.Constant,unit>()
let listConstant = (between (pstring "[" >>. ws) (pstring "]" >>. ws) (many (constantValue .>> ws .>> opt listSeparator) .>> ws)) .>> ws |>> ThriftAST.ListConstant
let mapConstant = 
    let keyValue = (constantValue .>> pstring ":" .>>. constantValue .>> opt listSeparator) .>> ws
    (between (pstring "{" >>. ws) (pstring "}" >>. ws) (many keyValue)) .>> ws |>> ThriftAST.MapConstant
do constantValueRef := doubleConstant <|> intConstant <|> literalConstant <|> identifierConstant <|> listConstant <|> mapConstant

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
let mapType = pstring "map" >>. (between (pstring "<" >>. ws) (pstring ">" >>. ws) (fieldType .>> ws .>> pstring "," .>> ws .>>. fieldType .>> ws)) .>> ws |>> ThriftAST.ContainerType.Map
let setType = pstring "set" >>. (between (pstring "<" >>. ws) (pstring ">" >>. ws) (fieldType .>> ws)) .>> ws |>> ThriftAST.ContainerType.Set
let listType = pstring "list" >>. (between (pstring "<" >>. ws) (pstring ">" >>. ws) (fieldType .>> ws)) .>> ws |>> ThriftAST.ContainerType.List
let containerType = (mapType <|> setType <|> listType) .>> ws
let definitionType = ((baseType |>> ThriftAST.BaseDefinition) <|> (containerType |>> ThriftAST.ContainerDefinition) .>> ws)
do fieldTypeRef := ((baseType |>> ThriftAST.BaseField) <|> (containerType |>> ThriftAST.ContainerField) <|> (identifier |>> ThriftAST.IdentifierField)) .>> ws


// Fields
let fieldId = (intConstant >>= (function | ThriftAST.IntegerConstant v -> preturn v | _ -> fail "expected integer constant")) .>> pstring ":" .>> ws
let requiredness = 
    (pstring "required" .>> ws >>. preturn true) <|> 
    (pstring "optional" .>> ws >>. preturn false)
let field = 
    (opt fieldId) .>>. (opt requiredness) .>>. fieldType .>> ws .>>. identifier .>> ws .>>. opt(pstring "=" >>. ws >>. constantValue) .>> ws .>> opt (listSeparator) .>> ws
        |>> (fun ((((id,req),t),i),v) -> ThriftAST.Field (id,req,t,i,v))

// Functions
let throws = pstring "throws" >>.ws >>. (between (pstring "(" >>. ws) (pstring ")" >>. ws) (many field .>> ws)) .>> ws |>> ThriftAST.Throws
let functionType = 
    ((pstring "void" >>. ws >>. preturn ThriftAST.FunctionType.Void) <|>
     (fieldType .>> ws |>> ThriftAST.FunctionType.Type)) .>> ws
let functionDef = 
    let isOneway = (opt ((pstring "oneway" .>> ws) >>. preturn true)) .>> ws
    isOneway .>>. functionType
             .>>. identifier .>> ws
             .>>. (between (pstring "(" >>. ws) (pstring ")" >>. ws) (many (field .>> ws))) 
             .>> ws 
             .>>. opt (throws .>> ws) 
             .>> opt listSeparator 
             .>> ws
        |>> (fun ((((o,t),i),f),tr) ->  ThriftAST.Function (o,t,i,f,tr))

let bracketed ident contents = 
    ident >>. ws 
          >>. identifier 
          .>> ws 
          .>>. (between (pstring "{" .>> ws) (pstring "}" .>> ws) contents) 
          .>> ws


// Service
let serviceDef = 
    let servicedecl = pstring "service" >>. ws >>. identifier .>> ws .>>. (opt (pstring "extends" >>. ws >>. identifier)) 
    servicedecl .>> ws .>>. (between (pstring "{" >>. ws) (pstring "}" >>. ws) (many functionDef)) .>> ws 
        |>> (fun ((n,e),f) -> ThriftAST.Service (n,e,f))

// Exception
let exceptionDef =
    bracketed (pstring "exception") (many field) |>> ThriftAST.Exception

// Union
let unionDef = 
    bracketed (pstring "union") (many field) |>> ThriftAST.Union

// Struct
let structDef = 
    bracketed (pstring "struct") (many field) |>> ThriftAST.Struct

// Enum
let enumDef =
    let valueDef = identifier .>> ws .>>. (opt (pstring "=" >>. ws >>. pint64 .>> ws)) .>> opt listSeparator .>> ws
    bracketed (pstring "enum") (many valueDef) |>> ThriftAST.Enum

// Typedef
let typeDef = pstring "typedef" >>. ws >>. definitionType .>>. identifier |>> ThriftAST.TypeDef

// Const
let constDef = 
    pstring "const" >>. ws >>. fieldType .>> ws .>>. identifier .>> ws .>>. (pstring "=" >>. ws >>. constantValue) .>> opt listSeparator .>> ws
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
    (manySatisfy (isLetter <||> isDigit <||> ((=) '-') <||> ((=) '_')) |>> ThriftAST.NamespaceScope.Other)

let namespaceDef = pstring "namespace" >>. ws 
                                >>. namespaceScope .>>. (opt (pstring "." >>. (manySatisfy (isLetter <||> isDigit <||> ((=) '_') <||> ((=) '-'))))) .>> ws 
                                .>>. identifier |>> (fun ((s,o),n) -> ThriftAST.Namespace (s,o,n))


// Include
let includeDef = (pstring "include" >>. ws >>. literal |>> ThriftAST.Include) <|> (pstring "cpp_include" >>. ws >>. literal |>> ThriftAST.Include)

// Header
let headerInfo = ((includeDef .>> ws |>> ThriftAST.IncludeHeader) <|> (namespaceDef .>> ws |>> ThriftAST.NamespaceHeader)) .>> ws

// Document
let document = (opt anyComment) >>. ws >>. (opt (many headerInfo)) .>> ws .>>. (opt (many definitionInfo))
                                .>> ws 
                                    |>> (function | None,None -> ThriftAST.Document ([],[]) 
                                                  | Some h,None -> ThriftAST.Document (h,[])
                                                  | None,Some d -> ThriftAST.Document ([],d) 
                                                  | Some h,Some d -> ThriftAST.Document (h,d))