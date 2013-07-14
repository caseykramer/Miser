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

let toString chars =
    match chars with
    | [] -> System.String.Empty
    | _ -> System.String( chars |> Array.ofSeq)

let asCharList (s:string) = 
    s.ToCharArray() |> List.ofArray 

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

let (|StringStartsWith|_|) prefix input = 
    match (asCharList input) with
    | StartsWith prefix (rest)-> Some (rest |> toString)
    | _ -> None

let (|StartsWithAny|_|) prefix = function 
    | r::rest when prefix |> List.exists ((=) r) -> (Some (r,rest))
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

let (|InlineComment|_|) input =
    match input with
    | [] -> None
    | line::rest ->
        match line with
        | AsCharList(StartsWith ['/';'/'] comment)
        | AsCharList(StartsWith ['#'] comment) -> 
            comment |> toString |> (fun c -> Ast.Comment c,rest) |> Some
        | _ -> None

let (| BlockComment|_|) lines =
    match lines |> List.head with
    | AsCharList(StartsWith ['/';'*'] comment) ->
        let rec getComment inLines comment =
            match inLines with
            | [] -> Some (Ast.CommentBlock (List.rev comment),[])
            | line::rest ->
                match parseBracketedBody ['*';'/'] [] line with
                | None -> getComment rest (line::comment)
                | Some (body,rest) ->  Some (Ast.CommentBlock (List.rev ((body |> toString)::comment)),rest)
        getComment (lines |> List.tail) [comment |> toString]
    | _ -> None

let (|StringLiteral|_|) input =
    match input with
    | Delimited ['"'] (literal,rest)
    | Delimited ['\''] (literal,rest) ->
        Some(Ast.StringLiteral(toString literal),rest |> toString)
    | _ -> None

let (|ThriftInclude|_|) input = 
    match input with
    | StringStartsWith (asCharList "import") (rest) ->
        match rest with
        | StringLiteral (literal,rest) -> Some <| ((Ast.Include literal),rest)
        | _ -> failwith "Error parsing import"
    | _ -> None

let namespaceScope (input:string) = 
    match input with
    | StringStartsWith ['*'] (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.Any,ident),rest)
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "cpp") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Cpp,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "java") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Java,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "py" ) (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Py,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "perl") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Perl,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "rb") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace (Ast.NamespaceScope.Rb,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "cocoa") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.Cocoa,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | StringStartsWith (asCharList "csharp") (rest) -> 
        match rest with
        | Identifier (ident,rest) -> Some(Ast.Namespace(Ast.NamespaceScope.CSharp,ident),rest )
        | _ -> failwith "Error parsing namespace"
    | _ -> None

let (|ThriftNamespace|_|) = function 
    | StringStartsWith (asCharList "namespace") namespaceDef ->
        match namespaceScope namespaceDef with
        | Some (ns,rest) -> Some(ns,rest)
        | None -> None
    | _ -> None

let rec header input headers =
    match input with
    | ThriftInclude (incl,rest) -> header rest (Ast.IncludeHeader incl::headers)
    | _ ->
        match input with
        | ThriftNamespace (namespc,rest) -> header rest (Ast.NamespaceHeader namespc::headers)
        | _ -> Some (List.rev headers,input)
let (|BaseType|_|) input = 
    match (asCharList input) with
    | StartsWith (asCharList "bool") rest   -> Some(Ast.BaseType.Bool,rest |> toString)
    | StartsWith (asCharList "byte") rest   -> Some (Ast.BaseType.Byte,rest |> toString)
    | StartsWith (asCharList "i16") rest    -> Some (Ast.BaseType.I16,rest |> toString)
    | StartsWith (asCharList "int") rest
    | StartsWith (asCharList "i32") rest    -> Some (Ast.BaseType.I32,rest |> toString)
    | StartsWith (asCharList "i64") rest    -> Some (Ast.BaseType.I64,rest |> toString)
    | StartsWith (asCharList "double") rest -> Some (Ast.BaseType.Double,rest |> toString)
    | StartsWith (asCharList "string") rest -> Some (Ast.BaseType.String,rest |> toString)
    | _ -> None
let rec (|FieldType|_|) input = 
    match input with
    | Identifier (ident,rest) -> Some (Ast.FieldType.IdentifierField ident,rest)
    | BaseType (baseType,rest) -> Some (Ast.FieldType.BaseField baseType,rest)
    | ContainerType (containerType,rest) -> Some (Ast.FieldType.ContainerField containerType,rest)
    | _ -> None
and (|ContainerType|_|) input = 
    match asCharList input with
    | StartsWith (asCharList "set") rest -> 
        match rest |> toString with
        | FieldType (ftype,rest) -> Some (Ast.ContainerType.Set(ftype),rest)
        | _ -> failwith "invalid syntax for set definition"
    | StartsWith (asCharList "list") rest ->
        match rest |> toString with
        | FieldType (ftype,rest) -> Some (Ast.ContainerType.List(ftype),rest)
        | _ -> failwith "invalid syntax for list definition"
    | StartsWith (asCharList "map") rest ->
        match rest |> toString with
        | FieldType (ftype1,rest) ->
            match rest with
            | FieldType (ftype2,rest) ->
                Some (Ast.ContainerType.Map(ftype1,ftype2),rest)
            | _ -> failwith "invalid syntax for map definition"
        | _ -> failwith "invalid syntax for map definition"
    | _ -> None

let (|NumberConstant|_|) (input:string) =
    match (asCharList input) with
    | StartsWithAny ('+'::'-'::numbers) (p,rest) ->
        let rec getConstantValue input value = 
            match input with
            | StartsWithAny ('.'::numbers) (p,rest) -> getConstantValue rest (p::value)
            | _ -> 
                let constValue:string = value |> List.rev |> toString
                if constValue.IndexOf('.') > 0 
                    then Some <| (Ast.ConstantValue.DoubleConstant(constValue |> doubleParse),rest)
                    else Some <| (Ast.ConstantValue.IntConstant(constValue |> intParse),rest)
        getConstantValue (asCharList input) []
    | _ -> None

        