module Parser

type ParseError<'a> = ParseError of 'a

module List = 
    let partitionWhile f = 
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let intParse = System.Int32.Parse
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
        | [],rest -> Some(toString rest)
        | _ -> None
    loop (prefix,input)

let (|StringStartsWith|_|) prefix input = 
    match (asCharList input) with
    | StartsWith prefix (rest)-> Some rest
    | _ -> None

let (|Value|_|) (v:string) (input:string) = 
    match input with
    | StringStartsWith (asCharList v) (rest) -> Some (rest)
    | _ -> None

let (|StartsWithAny|_|) prefix = function 
    | r::rest when prefix |> List.exists ((=) r) -> (Some (r,rest))
    | _ -> None

let (|ListSeparator|_|) input = 
    match input |> asCharList with
    | StartsWithAny [',';';'] (_,rest) -> Some (rest)
    | _ -> None

let rec private parseBracketedBody closing acc input = 
    match (asCharList input) with
    | StartsWith (closing) (rest) -> Some(List.rev acc,rest)
    | c::chars -> parseBracketedBody closing (c::acc) (chars |> toString)
    | _ -> None

let rec parseBracketed opening closing input = 
    match input with
    | StringStartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Bracketed|_|) ``begin`` ``end`` = parseBracketed ``begin`` ``end``

let (|Word|_|) = parseBracketed [' '] [' ']

let (|EOL|_|) = function
    | StartsWith ['\r';'\n'] (input) 
    | StartsWith ['\r'] (input) 
    | StartsWith ['\n'] (input) -> Some input
    | _ -> None

let (|InlineComment|_|) (input:char list) =
    match input |> toString with
    | Bracketed ['/';'/'] ['\r';'\n'] (comment,rest)
    | Bracketed ['/';'/'] ['\r']      (comment,rest)
    | Bracketed ['/';'/'] ['\n']      (comment,rest)
    | Bracketed ['#']     ['\r';'\n'] (comment,rest)
    | Bracketed ['#']     ['\r']      (comment,rest)
    | Bracketed ['#']     ['\n']      (comment,rest) -> Some <| (Ast.Comment(comment |> toString),rest) 
    | StringStartsWith ['#'] (comment)
    | StringStartsWith ['/';'/'] (comment) -> Some <| (Ast.Comment(comment),"") 
    |  _ -> None

let (| BlockComment|_|) (input:char list) =
    match input |> toString with
    | Bracketed ['/';'*'] ['*';'/'] (comment,rest) -> Some(Ast.CommentBlock(comment |> toString),rest)
    | _ -> None

let (|DocComment|_|) (input:string) =
    match input with
    | Bracketed ['/';'*';'*'] ['*';'/'] (comment,rest) -> Some(Ast.DocComment(comment |> toString),rest)
    | _ -> None

let rec (|WS|_|) (input:string) = 
    match asCharList input with
    | StartsWith [' '] (WS input)
    | StartsWith ['\t'] (WS input)
    | StartsWith ['#'] (WS input)
    | InlineComment (_, WS input)
    | BlockComment (_, WS input)
    | EOL (WS input) -> Some input
    | _ as input -> Some (toString input)

let (|PrefixedLines|) prefix (lines:string list) = 
    let prefixed, other = 
        lines |> List.partitionWhile (fun line ->
            line.StartsWith(prefix))
    [ for lines in prefixed ->
        lines.Substring(prefix.Length)], other

let (|LinesSeparated|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    match List.partitionWhile (isWhite >> not) lines with
    | par, _::rest
    | par, ([] as rest) -> par, rest

let listSeparators = [',';';']
let letters = ['A'..'Z'] @ ['a'..'z']
let numbers = ['0'..'9']

let (|Identifier|_|) (input:string) = 
    match (asCharList input) with
    | StartsWithAny ('_'::letters) (p,rest) ->
        let idetifierChars = letters@numbers@['_';'-';'.']
        let rec getIdentifier (input:char list) (identifier:char list) = 
            match input with
            | StartsWithAny idetifierChars (p,rest) -> getIdentifier rest (p::identifier)
            | _ -> Some(Ast.Identifier (List.rev identifier |> toString),input |> toString)
        getIdentifier rest [p]
    | _ -> None

let (|StringLiteral|_|) input =
    match input with
    | Delimited ['"'] (literal,rest)
    | Delimited ['\''] (literal,rest) ->
        Some(Ast.StringLiteral(toString literal),rest)
    | _ -> None

let (|ThriftInclude|_|) input = 
    match input with
    | WS (StringStartsWith (asCharList "import") (rest)) ->
        match rest with 
        | (WS (StringLiteral (literal,rest))) -> Some <| ((Ast.Include literal),rest)
        | _ -> failwith "Invalid include definition"
    | _ -> None

let namespaceScope (input:string) = 
    match input with
    | WS (StringStartsWith ['*']                     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Any,ident),rest)
    | WS (StringStartsWith (asCharList "cpp")        (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Cpp,ident),rest)
    | WS (StringStartsWith (asCharList "java")       (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Java,ident),rest)
    | WS (StringStartsWith (asCharList "py.twisted") (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Py_Twisted,ident),rest)
    | WS (StringStartsWith (asCharList "py" )        (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Py,ident),rest)
    | WS (StringStartsWith (asCharList "perl")       (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Perl,ident),rest)
    | WS (StringStartsWith (asCharList "rb")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Rb,ident),rest)
    | WS (StringStartsWith (asCharList "cocoa")      (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Cocoa,ident),rest)
    | WS (StringStartsWith (asCharList "csharp")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.CSharp,ident),rest)
    | WS (StringStartsWith (asCharList "c_glib")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.C_Glib,ident),rest)
    | WS (StringStartsWith (asCharList "go")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Go,ident),rest)
    | WS (StringStartsWith (asCharList "delphi")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Delphi,ident),rest)
    | WS (StringStartsWith (asCharList "js")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Javascript,ident),rest)
    | WS (StringStartsWith (asCharList "st")         (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Smalltalk,ident),rest)
    | WS (Identifier(nspace,                         (WS (Identifier (ident,rest)))))-> Some(Ast.Namespace(Ast.NamespaceScope.Other(nspace.Display),ident),rest)
    | _ -> None

let (|ThriftNamespace|_|) input =
    match input with
    | WS (StringStartsWith (asCharList "namespace") namespaceDef) ->
        match namespaceScope namespaceDef with
        | Some (ns,rest) -> Some(ns,rest)
        | _ -> failwith <| sprintf "Error parsing namespace at: %s" input
    | _ -> None


let (|ThriftHeader|_|) input = 
    let rec header input headers =
        match input with
        | ThriftInclude (incl,rest) -> header rest (Ast.IncludeHeader incl::headers)
        | ThriftNamespace (namespc,rest) -> header rest (Ast.NamespaceHeader namespc::headers)
        | _ -> Some (List.rev headers,input)
    header input List.empty

let (|BaseType|_|) input = 
    match (asCharList input) with
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
    | BaseType (baseType,rest) -> Some (Ast.FieldType.BaseField baseType,rest)
    | ContainerType (containerType,rest) -> Some (Ast.FieldType.ContainerField containerType,rest)
    | Identifier (ident,rest) -> Some (Ast.FieldType.IdentifierField ident,rest)
    | _ -> None
and (|ContainerType|_|) input = 
    match input with
    | StringStartsWith (asCharList "set") rest ->
        match parseBracketed ['<'] ['>'] rest with
        | Some (text,rest) -> 
            match text |> toString with
            | FieldType (ftype,_) -> Some (Ast.ContainerType.Set(ftype),rest)
            | _ -> None
        | None -> None
    | StringStartsWith (asCharList "list") rest ->
        match parseBracketed ['<'] ['>'] rest with
        | Some (text,rest) ->
            match text |> toString with
            | (FieldType (ftype,_)) -> Some (Ast.ContainerType.List(ftype),rest)
            | _ -> None
        | _ -> None
    | StringStartsWith (asCharList "map") rest ->
        match parseBracketed ['<'] ['>'] rest with
        | Some (text,rest) ->
            match text |> toString with
            | (FieldType (ftype1,other)) ->
                match asCharList other with
                | StartsWithAny (listSeparators) (_,remaining) ->
                    match remaining |> toString with
                    | (FieldType(ftype2,_)) -> Some (Ast.ContainerType.Map(ftype1,ftype2),rest)
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|DefinitionType|_|) (input:string) =
    match input with
    | WS (BaseType (baseType,rest)) -> Some (Ast.BaseDefinition(baseType),rest)
    | WS (ContainerType (container,rest)) -> Some (Ast.ContainerDefinition(container),rest)
    | _ -> None

let (|Number|_|) (input:string) =
    match (asCharList input) with
    | StartsWithAny (numbers) (p,rest) ->
        let rec getNumberValue input value = 
            match input with
            | StartsWithAny (numbers) (p,rest) -> getNumberValue rest (p::value)
            | _ ->
                let constValue:string = value |> List.rev |> toString
                (intParse constValue,input |> toString)
        Some (getNumberValue rest [p])
    | _ -> None

let (|NumberConstant|_|) (input:string) =
    match (asCharList input) with
    | StartsWithAny ('+'::'-'::numbers) (p,rest) ->
        let applySignInt value = 
            match p with
            | '-' -> value * -1
            | _ -> value
        let applySignDouble value = 
            match p with
            | '-' -> value * -1.0
            | _ -> value
        let rec getConstantValue input value = 
            match input with
            | StartsWithAny ('.'::numbers) (p,rest) -> getConstantValue rest (p::value)
            | _ -> 
                let constValue:string = value |> List.rev |> toString
                if constValue.IndexOf('.') > 0 
                    then Some <| (Ast.ConstantValue.DoubleConstant(constValue |> doubleParse),rest |> toString)
                    else Some <| (Ast.ConstantValue.IntConstant(constValue |> intParse),rest |> toString)
        getConstantValue (rest) [p]
    | _ -> None

let rec (|ConstList|_|) (input:string) = 
    match input with
    | Bracketed ['['] [']'] (vals,rest) ->
        let rec getConstStringValue input values = 
            match input with
            | "" -> values |> List.rev
            | ConstValue (cvalue,(WS (ListSeparator(rest)))) -> getConstStringValue (rest |> toString) (cvalue::values)
            | ConstValue (cvalue,rest) -> cvalue::values |> List.rev 
            | _ -> failwith "Invalid List Literal syntax: '%s'" vals
        let values = getConstStringValue (vals |> toString) []
        Some (Ast.ConstantValue.ListConstant(values),rest)
    | Bracketed ['{'] ['}'] (vals,rest) ->
        let rec getConstMapValue input values = 
            match input with
            | "" -> values |> List.rev |> Map.ofList
            | ConstValue (cvalueKey,(WS (StringStartsWith [':'] (ConstValue (cvalueValue,(WS(ListSeparator(rest)))))))) -> getConstMapValue (rest |> toString) ((cvalueKey,cvalueValue)::values)
            | ConstValue (cvalueKey,(WS (StringStartsWith [':'] (ConstValue (cvalueValue,rest))))) -> ((cvalueKey,cvalueValue)::values) |> List.rev |> Map.ofList
            | _ -> failwith "Invalid Map Literal syntax: '%s'" vals
        let values = getConstMapValue (vals |> toString) []
        Some (Ast.ConstantValue.MapConstant(values),rest)
    | _ -> None
and (|ConstValue|_|) (input:string) = 
    match input with
        | WS (StringLiteral(sliteral,rest)) -> Some <| (Ast.ConstantValue.LiteralConstant(sliteral),rest)
        | WS (NumberConstant(nconst,rest)) -> Some <| (nconst,rest)
        | WS (Identifier(ident,rest)) -> Some <| (Ast.ConstantValue.IdentConstant(ident),rest)
        | WS (ConstList(clist,rest)) -> Some <| (clist,rest)
        | _ -> None
let (|Constant|_|) (input:string) = 
    match input with
    | StringStartsWith (asCharList "const") (WS (FieldType(ftype,WS (Identifier(ident,ConstValue(cval,rest)))))) -> Some <| (Ast.ConstDefinition(Ast.Const(ftype,ident,cval)),rest)        
    | _ -> None
        
let (|TypeDef|_|) (input:string) =
    match input with 
    | StringStartsWith (asCharList "typedef") (DefinitionType (definition,WS (Identifier(ident,rest)))) -> Some (Ast.TypeDef(definition,ident),rest)
    | _ -> None

let (|BracketedDefinition|_|) defIdentifier (input:string) = 
    match input with
    | StringStartsWith (asCharList defIdentifier) (WS (Identifier(ident,rest))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,vals,rest)
        | _ -> None
    | _ -> None

let (|Enum|_|) (input:string) = 
    match input with
    | BracketedDefinition "enum" (ident,vals,rest) ->
        let rec getEnumBody input values counter = 
            match input with
            | WS ("") -> values |> List.rev
            | WS (Identifier (ident,WS (StringStartsWith ['='] (WS (Number (num,WS (ListSeparator (rest)))))))) ->
                getEnumBody (rest |> toString) ((num,ident)::values) (num+1)
            | WS (Identifier (ident,WS (StringStartsWith ['='] (WS (Number (num,rest)))))) ->
                getEnumBody rest ((num,ident)::values) (num+1)                    
            | WS (Identifier (ident,WS (ListSeparator (rest)))) -> getEnumBody (rest |> toString) ((counter,ident)::values) (counter+1)
            | WS (Identifier (ident,rest)) -> ((counter,ident)::values) |> List.rev
            | _ -> failwithf "Invalid enum body format '%s' at '%s'" (vals |> toString) input
        let values = getEnumBody (vals |> toString) [] 0
        Some <| (Ast.EnumDefinition(Ast.Enum(ident,values)),rest)        
    | _ -> None

let (|FieldDef|_|) input =
    match input with
    | WS (Value "optional" (WS (FieldType (field, WS (Identifier (ident,WS (Value "=" (ConstValue (cnst,rest))))))))) -> Some <| (Ast.OptionalField(Ast.Field (field,ident,Some cnst)),rest)
    | WS (Value "optional" (WS (FieldType (field, WS (Identifier (ident,rest)))))) -> Some <| (Ast.OptionalField(Ast.Field (field,ident,None)),rest)    
    | WS (FieldType (field, WS (Identifier (ident,WS (Value "=" (WS (ConstValue (cnst,rest)))))))) -> Some <| (Ast.RequiredField(Ast.Field (field,ident,Some cnst)),rest)
    | WS (FieldType (field, WS (Identifier (ident,rest)))) -> Some <| (Ast.RequiredField(Ast.Field (field,ident,None)),rest)
    | _ -> None

let (|NumberedField|_|) input = 
    match input with
    | WS (Number (fieldNum,(WS (Value ":" (FieldDef (field, WS (ListSeparator (rest)))))))) -> Some <| (Ast.NumberedField(fieldNum,field),rest |> toString)
    | WS (Number (fieldNum,(WS (Value ":" (FieldDef (field,rest)))))) -> Some <| (Ast.NumberedField(fieldNum,field),rest)
    | _ -> None

let (|UnnumberedField|_|) input = 
    match input with
    | WS (FieldDef (field,WS(ListSeparator (rest)))) -> Some <| (field,rest |> toString)
    | WS (FieldDef (field,rest)) -> Some <| (field,rest)
    | _ -> None

let (|ExtractField|_|) input = 
    match input with
    | NumberedField (field,rest) -> Some (field,rest)
    | UnnumberedField (field,rest) -> Some (field,rest)
    | _ -> None

let rec private getStructBody body input fields = 
    match input with 
    | WS ("") -> fields |> List.rev
    | ExtractField (field,rest) -> getStructBody body rest (field::fields)
    | _ -> failwithf "Invalid body: '%s' at '%s'" (body) input
        

let (|Struct|_|) (input:string) = 
    match input with
    | BracketedDefinition "struct" (ident,body,rest) ->
        let fields = getStructBody (body |> toString) (body |> toString) []
        Some <| (Ast.StructDefinition(Ast.Struct(ident,fields)),rest)
    | _ -> None

// Exceptions are basically structs by another name
let (|Exception|_|) (input:string) =
    match input with
    | BracketedDefinition "exception" (ident,body,rest) ->
        let fields = getStructBody (body |> toString) (body |> toString) []
        Some <| (Ast.ExceptionDefinition(Ast.Exception(ident,fields)),rest)
    | _ -> None

// Unions are syntactically just like structs, only the fields are not technically required. 
// This gets enforced when interpreting the AST, though, not when building it
let (|Union|_|) (input:string) = 
    match input with
    | BracketedDefinition "union" (ident,body,rest) ->
        let fields = getStructBody (body |> toString) (body |> toString) []
        Some <| (Ast.UnionDefinition(Ast.Union(ident,fields)),rest)
    | _ -> None

let (|FunctionType|_|) (input:string) = 
    match input with
    | WS (Value "void" (rest)) -> Some(Ast.VoidFunction,rest)
    | WS (FieldType (ftype,rest)) -> Some ((fun (a,b,c) -> Ast.Function(ftype,a,b,c)),rest)
    | _ -> None

let (|Throws|_|) (input:string) = 
    match input with
    | WS (Value "throws" (WS (Bracketed ['('] [')'] (vals,rest)))) ->
        let fields = getStructBody (vals |> toString) (vals |> toString) []
        Some (fields,rest)
    | _ -> None

let (|Function|_|) (input:string) = 
    match input with
    | FunctionType(func,WS (Identifier(fName,(WS (Bracketed ['('] [')'] (vals, WS (Throws (excepts,rest)))))))) ->
        let parameters = getStructBody (vals |> toString) (vals |> toString) []
        Some <| (func(fName,parameters,excepts),rest)
    | FunctionType(func,WS (Identifier(fName,(WS (Bracketed ['('] [')'] (vals,rest)))))) ->
        let parameters = getStructBody (vals |> toString) (vals |> toString) []
        Some <| (func(fName,parameters,[]),rest)
    | _ -> None
    

let rec getServiceBody body input functions = 
    match input with
    | WS("") -> functions |> List.rev
    | WS (Function (func,WS (ListSeparator (rest)))) -> getServiceBody body (rest |> toString) (func::functions)
    | WS (Function (func,rest)) -> getServiceBody body rest (func::functions)
    | _ -> failwithf "Invalid service body: '%s' at '%s'" body input

let (|ServiceDef|_|) (input:string) = 
    match input with
    | StringStartsWith (asCharList "service") (WS (Identifier(ident,WS (Value "extends" (WS (Identifier (parent,rest))))))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,Some parent,vals,rest)
        | _ -> None
    | StringStartsWith (asCharList "service") (WS (Identifier(ident,rest))) ->
        match rest with
        | WS (Bracketed ['{'] ['}'] (vals,rest)) -> Some (ident,None,vals,rest)
        | _ -> None
    | _ -> None

let (|Service|_|) (input:string) = 
    match input with
    | ServiceDef (serviceName,parent,body,rest) ->
        let functions = getServiceBody (body |> toString) (body |> toString) []
        Some <| (Ast.ServiceDefinition(Ast.Service(serviceName,functions,parent)),rest)
    | _ -> None
    