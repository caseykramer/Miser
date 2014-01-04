module Parser

type ParseError<'a> = ParseError of 'a

module List = 
    let partitionWhile f = 
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let intParse = System.Int32.Parse
let intParseHex num = System.Int32.Parse(num,System.Globalization.NumberStyles.HexNumber)

let doubleParse = System.Double.Parse
let intTryParse s = match System.Int32.TryParse(s) with
                    | (true,i) -> Some i
                    | _ -> None
let doubleTryParse s = match System.Double.TryParse(s) with
                       | (true,d) -> Some d
                       | _ -> None
let EOF = "".ToCharArray() |> List.ofArray

let toString chars =
    match chars with
    | [] -> System.String.Empty
    | _ -> System.String( chars |> Array.ofSeq)

let asCharList (s:string) = 
    s.ToCharArray() |> List.ofArray 

let (|AsCharList|) (str:string) = 
    List.ofSeq str

let toLines (s:string) = 
    s.Split([|'\r';'\n'|]) |> List.ofArray |> List.filter (fun s -> not(System.String.IsNullOrWhiteSpace(s)))

let (|EOF|) = function
    | [] -> Some()
    | _-> None

let (|StartsWith|_|) prefix (input:char list) = 
    let rec loop = function
        | p::prefix,r::rest when p = r ->
            loop (prefix,rest)
        | [],rest -> Some(rest)
        | _ -> None
    loop (prefix,input)

let (|Value|_|) (v:string) (input:char list) = 
    match input with
    | StartsWith (asCharList v) (rest) -> Some (rest)
    | _ -> None

let (|StartsWithAny|_|) prefix = function 
    | r::rest when prefix |> List.exists ((=) r) -> (Some (r,rest))
    | _ -> None


let (|ListSeparator|_|) input = 
    match input with
    | StartsWithAny [',';';'] (_,rest) -> Some (rest)
    | _ -> None

let (|EOL|_|) = function
    | StartsWith ['\r';'\n'] (input) 
    | StartsWith ['\r'] (input) 
    | StartsWith ['\n'] (input) -> Some input
    | _ -> None

let rec private parseBracketedBody opening closing acc input nestingLevel = 
    match input with
    | StartsWith ['/';'/'] (rest)
    | StartsWith ['#'] (rest) ->
        let rec getComment chars comment = 
            match chars with
            | [] -> (comment,[])
            | EOL (rest) -> ('\n'::'\r'::comment,rest)
            | c::chars -> getComment chars (c::comment)
        let (comment,rest) = getComment (rest) ['/';'/']
        parseBracketedBody opening closing (comment@acc) (rest) nestingLevel
    | StartsWith ['/';'*'] (rest) ->
        let rec getComment chars comment =
            match chars with
            | [] -> ('/'::'*'::comment,[])
            | StartsWith ['*';'/'] (rest) -> ('/'::'*'::comment,rest)
            | c::chars -> getComment chars (c::comment)
        let (comment,rest) = getComment (rest) ['*';'/']
        parseBracketedBody opening closing (comment@acc) (rest) nestingLevel
    | StartsWith (opening) (rest) when opening <> closing -> parseBracketedBody opening closing ((opening |> List.rev)@acc) rest (nestingLevel + 1)
    | StartsWith (closing) (rest) when nestingLevel <= 0 -> Some(List.rev acc,rest)
    | StartsWith (closing) (rest) -> parseBracketedBody opening closing ((closing |> List.rev)@acc) rest (nestingLevel - 1)
    | c::chars -> parseBracketedBody opening closing (c::acc) chars nestingLevel
    | _ -> None

let rec parseBracketed opening closing input = 
    match input with
    | StartsWith opening chars ->
        parseBracketedBody opening closing [] chars 0
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Bracketed|_|) ``begin`` ``end`` = parseBracketed ``begin`` ``end``

let (|InlineComment|_|) input =
    let rec getComment chars comment = 
        match chars with
        | [] -> (Ast.Comment(comment |> List.rev |> toString),[])
        | EOL (rest) -> (Ast.Comment(comment |> List.rev |> toString),chars)
        | c::chars -> getComment chars (c::comment)
        
    match input with
    | StartsWith ['/';'/'] (rest)
    | StartsWith ['#'] (rest) -> Some (getComment rest [])
    |  _ -> None

let (|BlockComment|_|) input =
    let rec getComment chars comment = 
        match chars with
        | [] -> (Ast.CommentBlock(comment |> List.rev |> toString),[])
        | StartsWith ['*';'/'] (rest) -> (Ast.CommentBlock(comment |> List.rev |> toString),rest)
        | c::chars -> getComment chars (c::comment)
    match input with
    | StartsWith ['/';'*'] (rest) -> Some <| getComment rest []
    | _ -> None

let (|DocComment|_|) input =
    let rec getComment chars comment = 
        match chars with
        | [] -> (Ast.DocComment(comment |> List.rev |> toString),[])
        | StartsWith ['*';'/'] (rest) -> (Ast.DocComment(comment |> List.rev |> toString),rest)
        | c::chars -> getComment chars (c::comment)
    match input with
    | StartsWith ['/';'*';'*'] (rest) -> Some <| (getComment rest [])
    | _ -> None

let rec (|WS|_|) input = 
    match input with
    | StartsWith [' '] (WS input)
    | StartsWith ['\t'] (WS input)
    | StartsWith ['#'] (WS input)
    | InlineComment (_, WS input)
    | BlockComment (_, WS input)
    | EOL (WS input) -> Some input
    | _ as input -> Some input

let listSeparators = [',';';']
let letters = ['A'..'Z'] @ ['a'..'z']
let numbers = ['0'..'9']
let hexnumbers = ['0'..'9'] @ ['a'..'f'] @ ['A'..'F']

let (|Identifier|_|) input = 
    match input with
    | StartsWithAny ('_'::letters) (p,rest) ->
        let idetifierChars = letters@numbers@['_';'-';'.']
        let rec getIdentifier (input:char list) (identifier:char list) = 
            match input with
            | StartsWithAny idetifierChars (p,rest) -> getIdentifier rest (p::identifier)
            | _ -> Some(Ast.Identifier (List.rev identifier |> toString),input)
        getIdentifier rest [p]
    | _ -> None

let (|BoolLiteral|_|) input = 
    match input with
    | StartsWith (asCharList "true") (rest) -> Some(Ast.BoolLiteral(true),rest)
    | StartsWith (asCharList "false") (rest) -> Some(Ast.BoolLiteral(false),rest)
    | _ -> None

let (|StringLiteral|_|) input =
    match input with
    | Delimited ['"'] (literal,rest)
    | Delimited ['\''] (literal,rest) ->
        Some(Ast.StringLiteral(toString literal),rest)
    | _ -> None

let (|ThriftInclude|_|) input = 
    match input with
    | WS (StartsWith (asCharList "import") (rest)) ->
        match rest with 
        | (WS (StringLiteral (literal,rest))) -> Some <| ((Ast.Include literal),rest)
        | _ -> failwith "Invalid include definition"
    | _ -> None

let namespaceScope input = 
    match input with
    | WS (StartsWith ['*']                     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Any,ident),rest)
    | WS (StartsWith (asCharList "cpp")        (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Cpp,ident),rest)
    | WS (StartsWith (asCharList "java")       (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Java,ident),rest)
    | WS (StartsWith (asCharList "py.twisted") (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Py_Twisted,ident),rest)
    | WS (StartsWith (asCharList "py" )        (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Py,ident),rest)
    | WS (StartsWith (asCharList "perl")       (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Perl,ident),rest)
    | WS (StartsWith (asCharList "rb")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Rb,ident),rest)
    | WS (StartsWith (asCharList "cocoa")      (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Cocoa,ident),rest)
    | WS (StartsWith (asCharList "csharp")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.CSharp,ident),rest)
    | WS (StartsWith (asCharList "c_glib")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.C_Glib,ident),rest)
    | WS (StartsWith (asCharList "go")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Go,ident),rest)
    | WS (StartsWith (asCharList "delphi")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Delphi,ident),rest)
    | WS (StartsWith (asCharList "js")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Javascript,ident),rest)
    | WS (StartsWith (asCharList "st")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Smalltalk,ident),rest)
    | WS (StartsWith (asCharList "php")        (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Php,ident),rest)
    | WS (Identifier(nspace,                   (WS (Identifier (ident,rest)))))-> Some(Ast.Namespace(Ast.NamespaceScope.Other(nspace.Display),ident),rest)
    | _ -> None

let (|ThriftNamespace|_|) input =
    match input with
    | WS (StartsWith (asCharList "namespace") namespaceDef) ->
        match namespaceScope namespaceDef with
        | Some (ns,rest) -> Some(ns,rest)
        | _ -> failwith <| sprintf "Error parsing namespace at: %s" (toString input)
    | _ -> None


let (|ThriftHeader|_|) input = 
    let rec header input headers =
        match input with
        | ThriftInclude (incl,rest) -> header rest (Ast.IncludeHeader incl::headers)
        | ThriftNamespace (namespc,rest) -> header rest (Ast.NamespaceHeader namespc::headers)
        | _ -> Some (List.rev headers,input)
    header input List.empty

let (|BaseType|_|) input = 
    match input with
    | StartsWith (asCharList "binary") rest -> Some (Ast.BaseType.Binary,rest)
    | StartsWith (asCharList "bool") rest   -> Some (Ast.BaseType.Bool,rest)
    | StartsWith (asCharList "byte") rest   -> Some (Ast.BaseType.Byte,rest)
    | StartsWith (asCharList "i16") rest    -> Some (Ast.BaseType.I16,rest)
    | StartsWith (asCharList "int") rest
    | StartsWith (asCharList "i32") rest    -> Some (Ast.BaseType.I32,rest)
    | StartsWith (asCharList "i64") rest    -> Some (Ast.BaseType.I64,rest)
    | StartsWith (asCharList "double") rest -> Some (Ast.BaseType.Double,rest)
    | StartsWith (asCharList "string") rest -> Some (Ast.BaseType.String,rest)
    | _ -> None
let rec (|FieldType|_|) input = 
    match input with
    | ContainerType (containerType,rest) -> Some (Ast.FieldType.ContainerField containerType,rest)
    | BaseType (baseType,rest) -> Some (Ast.FieldType.BaseField baseType,rest)
    | Identifier (ident,rest) -> Some (Ast.FieldType.IdentifierField ident,rest)
    | _ -> None
and (|ContainerType|_|) input = 
    match input with
    | WS(StartsWith (asCharList "set") rest) ->
        match rest with
        | WS(Bracketed ['<'] ['>'] (text,rest)) -> 
            match text with
            | WS(FieldType (ftype,_)) -> Some (Ast.ContainerType.Set(ftype),rest)
            | _ -> None
        | _ -> None
    | WS(StartsWith (asCharList "list") rest) ->
        match rest with
        | WS(Bracketed ['<'] ['>'] (text,rest)) ->
            match text with
            | WS(FieldType (ftype,_)) -> Some (Ast.ContainerType.List(ftype),rest)
            | _ -> None
        | _ -> None
    | WS(StartsWith (asCharList "map") rest) ->
        match rest with
        | WS (Bracketed ['<'] ['>'] (text,rest)) ->
            match text with
            | WS(FieldType (ftype1,other)) ->
                match other with
                | WS(StartsWithAny (listSeparators) (_,remaining)) ->
                    match remaining with
                    | WS(FieldType(ftype2,_)) -> Some (Ast.ContainerType.Map(ftype1,ftype2),rest)
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|DefinitionType|_|) input =
    match input with
    | WS (BaseType (baseType,rest)) -> Some (Ast.BaseDefinition(baseType),rest)
    | WS (ContainerType (container,rest)) -> Some (Ast.ContainerDefinition(container),rest)
    | _ -> None

let (|Number|_|) input =
    match input with
    | StartsWith ['0';'x'] (rest) ->
        let rec getHexNumberValue input value = 
            match input with
            | StartsWithAny (hexnumbers) (p,rest) -> getHexNumberValue rest (p::value)
            | _ ->
                let constValue:string = value |> List.rev |> toString
                (intParseHex constValue,input)
        Some (getHexNumberValue rest [])
    | StartsWithAny (numbers) (p,rest) ->
        let rec getNumberValue input value = 
            match input with
            | StartsWithAny (numbers) (p,rest) -> getNumberValue rest (p::value)
            | _ ->
                let constValue:string = value |> List.rev |> toString
                (intParse constValue,input)
        Some (getNumberValue rest [p])
    | _ -> None

let (|NumberConstant|_|) input =
    match input with
    | StartsWithAny ('+'::'-'::numbers) (p,rest) ->
        let rec getConstantValue input value = 
            match input with
            | StartsWithAny ('.'::numbers) (p,rest) -> getConstantValue rest (p::value)
            | _ -> 
                let constValue:string = value |> List.rev |> toString
                if constValue.IndexOf('.') > 0 
                    then Some <| (Ast.ConstantValue.DoubleConstant(constValue |> doubleParse),input)
                    else Some <| (Ast.ConstantValue.IntConstant(constValue |> intParse),input)
        getConstantValue (rest) [p]
    | _ -> None

let rec (|ConstList|_|) input = 
    match input with
    | WS (Bracketed ['['] [']'] (vals,rest)) ->
        let rec getConstStringValue input values = 
            match input with
            | [] -> values |> List.rev
            | ConstValue (cvalue,(WS (ListSeparator(rest)))) -> getConstStringValue rest (cvalue::values)
            | ConstValue (cvalue,rest) -> cvalue::values |> List.rev 
            | _ -> failwith "Invalid List Literal syntax: '%s'" vals
        let values = getConstStringValue vals []
        Some (Ast.ConstantValue.ListConstant(values),rest)
    | WS (Bracketed ['{'] ['}'] (vals,rest)) ->
        let rec getConstMapValue (input:char list) values = 
            match input with
            | [] -> values |> List.rev |> Map.ofList
            | ConstValue (cvalueKey,(WS (StartsWith [':'] (ConstValue (cvalueValue,(WS(ListSeparator(rest)))))))) -> getConstMapValue rest ((cvalueKey,cvalueValue)::values)
            | ConstValue (cvalueKey,(WS (StartsWith [':'] (ConstValue (cvalueValue,rest))))) -> ((cvalueKey,cvalueValue)::values) |> List.rev |> Map.ofList
            | _ -> failwith "Invalid Map Literal syntax: '%s'" vals
        let values = getConstMapValue vals []
        Some (Ast.ConstantValue.MapConstant(values),rest)
    | _ -> None
and (|ConstValue|_|) input = 
    match input with
        | WS (BoolLiteral(bliteral,rest)) -> Some <| (Ast.ConstantValue.LiteralConstant(bliteral),rest)
        | WS (StringLiteral(sliteral,rest)) -> Some <| (Ast.ConstantValue.LiteralConstant(sliteral),rest)
        | WS (NumberConstant(nconst,rest)) -> Some <| (nconst,rest)
        | WS (Identifier(ident,rest)) -> Some <| (Ast.ConstantValue.IdentConstant(ident),rest)
        | WS (ConstList(clist,rest)) -> Some <| (clist,rest)
        | _ -> None
let (|Constant|_|) input = 
    match input with
    | WS (StartsWith (asCharList "const") (WS (FieldType(ftype,WS (Identifier(ident,WS ( Value "=" (WS (ConstValue(cval,ListSeparator(rest))))))))))) -> Some <| (Ast.ConstDefinition(Ast.Const(ftype,ident,cval)),rest)
    | WS (StartsWith (asCharList "const") (WS (FieldType(ftype,WS (Identifier(ident,WS ( Value "=" (WS (ConstValue(cval,rest)))))))))) -> Some <| (Ast.ConstDefinition(Ast.Const(ftype,ident,cval)),rest)        
    | _ -> None
        
let (|TypeDef|_|) input =
    match input with 
    | WS (StartsWith (asCharList "typedef") (DefinitionType (definition,WS (Identifier(ident,rest))))) -> Some (Ast.TypeDef(definition,ident),rest)
    | _ -> None

let (|BracketedDefinition|_|) defIdentifier input = 
    match input with
    | StartsWith (asCharList defIdentifier) (WS (Identifier(ident,rest))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,vals,rest)
        | _ -> None
    | _ -> None

let (|Enum|_|) input = 
    match input with
    | WS (BracketedDefinition "enum" (ident,vals,rest)) ->
        let rec getEnumBody input values counter = 
            match input with
            | WS ([]) -> values |> List.rev
            | WS (Identifier (ident,WS (StartsWith ['='] (WS (Number (num,WS (ListSeparator (rest)))))))) ->
                getEnumBody rest ((num,ident)::values) (num+1)
            | WS (Identifier (ident,WS (StartsWith ['='] (WS (Number (num,rest)))))) ->
                getEnumBody rest ((num,ident)::values) (num+1)                    
            | WS (Identifier (ident,WS (ListSeparator (rest)))) -> getEnumBody rest ((counter,ident)::values) (counter+1)
            | WS (Identifier (ident,rest)) -> ((counter,ident)::values) |> List.rev
            | _ -> failwithf "Invalid enum body format '%s' at '%s'" (toString vals) (toString input)
        let values = getEnumBody vals [] 0
        Some <| (Ast.EnumDefinition(Ast.Enum(ident,values)),rest)        
    | _ -> None

let (|FieldDef|_|) input =
    match input with
    | WS (Value "optional" (WS (FieldType (field, WS (Identifier (ident,WS (Value "=" (ConstValue (cnst,rest))))))))) -> Some <| (Ast.OptionalField(Ast.Field (field,ident,Some cnst)),rest)
    | WS (Value "optional" (WS (FieldType (field, WS (Identifier (ident,rest)))))) -> Some <| (Ast.OptionalField(Ast.Field (field,ident,None)),rest)
    | WS (Value "required" (WS (FieldType (field, WS (Identifier (ident,WS (Value "=" (ConstValue (cnst,rest))))))))) -> Some <| (Ast.RequiredField(Ast.Field (field,ident,Some cnst)),rest)
    | WS (Value "required" (WS (FieldType (field, WS (Identifier (ident,rest)))))) -> Some <| (Ast.RequiredField(Ast.Field (field,ident,None)),rest)
    | WS (FieldType (field, WS (Identifier (ident,WS (Value "=" (WS (ConstValue (cnst,rest)))))))) -> Some <| (Ast.Field (field,ident,Some cnst),rest)
    | WS (FieldType (field, WS (Identifier (ident,rest)))) -> Some <| (Ast.Field (field,ident,None),rest)
    | _ -> None

let (|NumberedField|_|) input = 
    match input with
    | WS (Number (fieldNum,(WS (Value ":" (FieldDef (field, WS (ListSeparator (rest)))))))) -> Some <| (Ast.NumberedField(fieldNum,field),rest)
    | WS (Number (fieldNum,(WS (Value ":" (FieldDef (field,rest)))))) -> Some <| (Ast.NumberedField(fieldNum,field),rest)
    | _ -> None

let (|UnnumberedField|_|) input = 
    match input with
    | WS (FieldDef (field,WS(ListSeparator (rest)))) -> Some <| (field,rest)
    | WS (FieldDef (field,rest)) -> Some <| (field,rest)
    | _ -> None

let (|ExtractField|_|) input = 
    match input with
    | NumberedField (field,rest) -> Some (field,rest)
    | UnnumberedField (field,rest) -> Some (field,rest)
    | _ -> None

let rec private getStructBody body input fields = 
    match input with 
    | WS ([]) -> fields |> List.rev
    | ExtractField (field,rest) -> getStructBody body rest (field::fields)
    | _ -> failwithf "Invalid body: '%s' at '%s'" (body) (toString input)
        

let (|Struct|_|) input = 
    match input with
    | WS (BracketedDefinition "struct" (ident,body,rest)) ->
        let fields = getStructBody (body |> toString) body []
        Some <| (Ast.StructDefinition(Ast.Struct(ident,fields)),rest)
    | _ -> None

// Exceptions are basically structs by another name
let (|Exception|_|) input =
    match input with
    | WS (BracketedDefinition "exception" (ident,body,rest)) ->
        let fields = getStructBody (body |> toString) body []
        Some <| (Ast.ExceptionDefinition(Ast.Exception(ident,fields)),rest)
    | _ -> None

// Unions are syntactically just like structs, only the fields are not technically required. 
// This gets enforced when interpreting the AST, though, not when building it
let (|Union|_|) input = 
    match input with
    | WS (BracketedDefinition "union" (ident,body,rest)) ->
        let fields = getStructBody (body |> toString) body []
        Some <| (Ast.UnionDefinition(Ast.Union(ident,fields)),rest)
    | _ -> None

let rec (|FunctionType|_|) input = 
    match input with
    | WS (Value "void" (rest)) -> Some(Ast.VoidFunction,rest)
    | WS (Value "oneway" (FunctionType (ftype,rest))) -> Some((Ast.OnewayFunction << ftype),rest)
    | WS (FieldType (ftype,rest)) -> Some ((fun (a,b,c) -> Ast.Function(ftype,a,b,c)),rest)
    | _ -> None

let (|Throws|_|) input = 
    match input with
    | WS (Value "throws" (WS (Bracketed ['('] [')'] (vals,rest)))) ->
        let fields = getStructBody (vals |> toString) vals []
        Some (fields,rest)
    | _ -> None

let (|Function|_|) input = 
    match input with
    | FunctionType(func,WS (Identifier(fName,(WS (Bracketed ['('] [')'] (vals, WS (Throws (excepts,rest)))))))) ->
        let parameters = getStructBody (vals |> toString) vals []
        Some <| (func(fName,parameters,excepts),rest)
    | FunctionType(func,WS (Identifier(fName,(WS (Bracketed ['('] [')'] (vals,rest)))))) ->
        let parameters = getStructBody (vals |> toString) vals []
        Some <| (func(fName,parameters,[]),rest)
    | _ -> None
    

let rec getServiceBody body input functions = 
    match input with
    | WS([]) -> functions |> List.rev
    | WS (Function (func,WS (ListSeparator (rest)))) -> getServiceBody body rest (func::functions)
    | WS (Function (func,rest)) -> getServiceBody body rest (func::functions)
    | _ -> failwithf "Invalid service body: '%s' at '%s'" body (toString input)

let (|ServiceDef|_|) input = 
    match input with
    | WS (StartsWith (asCharList "service") (WS (Identifier(ident,WS (Value "extends" (WS (Identifier (parent,rest)))))))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,Some parent,vals,rest)
        | _ -> None
    | WS (StartsWith (asCharList "service") (WS (Identifier(ident,rest)))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,None,vals,rest)
        | _ -> None
    | _ -> None

let (|Service|_|) input = 
    match input with
    | ServiceDef (serviceName,parent,body,rest) ->
        let functions = getServiceBody (body |> toString) body []
        Some <| (Ast.ServiceDefinition(Ast.Service(serviceName,functions,parent)),rest)
    | _ -> None
    
let parseDocument (document:string) = 
    let rec parse input (doc:Ast.Document) = 
        match input with
        | WS ([]) -> { doc with Headers = doc.Headers |> List.rev; Definitions = doc.Definitions |> List.rev }
        | ThriftInclude (incl,rest) -> parse rest { doc with Headers = Ast.IncludeHeader(incl)::doc.Headers }
        | ThriftNamespace (nspace,rest) -> parse rest { doc with Headers = Ast.NamespaceHeader(nspace)::doc.Headers }
        | Constant (cnst,rest) -> parse rest { doc with Definitions = cnst::doc.Definitions }
        | TypeDef (tdef,rest) -> parse rest { doc with Definitions = Ast.Definition.TypeDefinition(tdef)::doc.Definitions }
        | Enum (enum,rest) -> parse rest { doc with Definitions = enum::doc.Definitions }
        | Struct (strct,rest) -> parse rest { doc with Definitions = strct::doc.Definitions }
        | Union (union,rest) -> parse rest { doc with Definitions = union::doc.Definitions }
        | Exception (excp,rest) -> parse rest { doc with Definitions = excp::doc.Definitions }
        | Service (svc,rest) -> parse rest { doc with Definitions = svc::doc.Definitions }
        | _ -> failwithf "Error parsing document '%s' at '%s'" document (toString input)
    parse (asCharList document) { Headers = []; Definitions = [] }