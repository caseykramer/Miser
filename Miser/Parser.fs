module Parser

type ParseError<'a> = ParseError of 'a

module List = 
    let partitionWhile f = 
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let toString chars =
    System.String( chars |> Array.ofSeq)

let asCharList (s:string) = 
    s.ToCharArray() |> List.ofArray 

let toLines (s:string) = 
    s.Split([|'\r';'\n'|]) |> List.ofArray |> List.filter (fun s -> not(System.String.IsNullOrWhiteSpace(s)))

let (|EOF|) = function
    | [] -> Some()
    | _-> None

let (|StartsWith|_|) prefix input = 
    let rec loop = function
        | p::prefix,r::rest when p = r ->
            loop (prefix,rest)
        | [],rest -> Some(rest)
        | _ -> None
    loop (prefix,input)

let (|StartsWithAny|_|) prefix = function 
    | r::rest when prefix |> List.exists ((=) r) -> (Some (r,rest))
    | _ -> None

let rec private parseBracketedBody closing acc = function
    | StartsWith closing (rest) -> Some(List.rev acc,rest)
    | c::chars -> parseBracketedBody closing (c::acc) chars
    | _ -> None

let rec parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Word|_|) = parseBracketed [' '] [' ']

let (|EOL|_|) = function
    | StartsWith ['\r';'\n'] (input) 
    | StartsWith ['\r'] (input) 
    | StartsWith ['\n'] (input) -> Some input
    | _ -> None

let rec (|WS|_|) = function
    | StartsWith [' '] (WS input)
    | StartsWith ['\t'] (WS input)
    | EOL (WS input) -> Some input
    | _ as input -> Some input

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

let (|AsCharList|) (str:string) = 
    List.ofSeq str

let listSeparators = [',';';']
let letters = ['A'..'Z'] @ ['a'..'z']
let numbers = ['0'..'9']

let (|Identifier|_|) input = 
    match input with
    | StartsWithAny ('_'::letters) (p,rest) ->
        let idetifierChars = letters@numbers@['_';'-';'.']
        let rec getIdentifier (input:char list) (identifier:char list) = 
            match input with
            | StartsWithAny idetifierChars (p,rest) -> getIdentifier rest (p::identifier)
            | _ -> Some(List.rev identifier,input)
        getIdentifier rest [p]
    | _ -> None

let (|InlineComment|_|) = function
    | AsCharList(StartsWith ['/';'/'] comment)
    | AsCharList(StartsWith ['#'] comment) -> 
        comment |> toString |> Some
    | _ -> None

let blockComment lines =
    match lines |> List.head with
    | AsCharList(StartsWith ['/';'*'] comment) ->
        let rec getComment inLines comment =
            match inLines with
            | [] -> Some (List.rev comment,[])
            | line::rest ->
                match parseBracketedBody ['*';'/'] [] (line |> asCharList) with
                | None -> getComment rest (line::comment)
                | Some (body,_) ->  Some (List.rev ((body |> toString)::comment),rest)
        getComment (lines |> List.tail) [comment |> toString]
    | _ -> None

let stringLiteral input =
    match input with
    | Delimited ['"'] (literal,rest)
    | Delimited ['\''] (literal,rest) ->
        Some(Ast.StringLiteral(toString literal),rest)
    | _ -> None
        
let identifier input =
    match input with
    | Identifier (ident,rest) -> Some(Ast.Identifier (ident |> toString),rest)
    | _ -> None

let comment line = 
    match line with
    | InlineComment (comment) -> Some <| Ast.Comment (comment |> toString)
    | _ -> None

let thriftInclude input = 
    match input with
    | StartsWith (asCharList "import") (rest) ->
        match stringLiteral rest with
        | Some (literal,rest) -> Some <| ((Ast.Include literal),rest)
        | None -> failwith "Error parsing import"
    | _ -> None

let namespaceScope input = 
    match input with
    | StartsWith ['*'] (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.Any,ident),rest)
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "cpp") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Cpp,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "java") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Java,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "py" ) (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Py,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "perl") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Perl,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "rb") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Rb,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "cocoa") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.Cocoa,ident),rest )
        | None -> failwith "Error parsing namespace"
    | StartsWith (asCharList "csharp") (rest) -> 
        match identifier rest with
        | Some (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.CSharp,ident),rest )
        | None -> failwith "Error parsing namespace"
    | _ -> None

let thriftNamespace = function 
    | StartsWith (asCharList "namespace") namespaceDef ->
        match namespaceScope namespaceDef with
        | Some (ns,rest) -> Some(ns,rest)
        | None -> None
    | _ -> None

let rec header input headers =
    match thriftInclude input with
    | Some (incl,rest) -> header rest (Ast.IncludeHeader incl::headers)
    | None ->
        match thriftNamespace input with
        | Some (namespc,rest) -> header rest (Ast.NamespaceHeader namespc::headers)
        | None -> Some (List.rev headers,input)
let baseType input = 
    match input with
    | StartsWith (asCharList "bool") rest   -> Some(Ast.BaseType.Bool,rest)
    | StartsWith (asCharList "byte") rest   -> Some (Ast.BaseType.Byte,rest)
    | StartsWith (asCharList "i16") rest    -> Some (Ast.BaseType.I16,rest)
    | StartsWith (asCharList "int") rest
    | StartsWith (asCharList "i32") rest    -> Some (Ast.BaseType.I32,rest)
    | StartsWith (asCharList "i64") rest    -> Some (Ast.BaseType.I64,rest)
    | StartsWith (asCharList "double") rest -> Some (Ast.BaseType.Double,rest)
    | StartsWith (asCharList "string") rest -> Some (Ast.BaseType.String,rest)
    | _ -> None
let rec fieldType input = 
    match identifier input with
    | Some (ident,rest) -> Some (Ast.FieldType.IdentifierField ident,rest)
    | _ -> match baseType input with
           | Some (baseType,rest) -> Some (Ast.FieldType.BaseField baseType,rest)
           | _ -> match containerType input with
                  | Some (containerType,rest) -> Some (Ast.FieldType.ContainerField containerType,rest)
                  | _ -> None
and containerType input = 
    match input with
    | StartsWith (asCharList "set") rest -> 
        match fieldType rest with
        | Some (ftype,rest) -> Some (Ast.ContainerType.Set(ftype),rest)
        | _ -> failwith "invalid syntax for set definition"
    | StartsWith (asCharList "list") rest ->
        match fieldType rest with
        | Some (ftype,rest) -> Some (Ast.ContainerType.List(ftype),rest)
        | _ -> failwith "invalid syntax for list definition"
    | StartsWith (asCharList "map") rest ->
        match fieldType rest with
        | Some (ftype1,rest) ->
            match fieldType rest with
            | Some (ftype2,rest) ->
                Some (Ast.ContainerType.Map(ftype1,ftype2),rest)
            | _ -> failwith "invalid syntax for map definition"
        | _ -> failwith "invalid syntax for map definition"
    | _ -> None

let thriftConst input = 
    | StartsWith (asCharList "const") constDef ->
        match constDef with
        