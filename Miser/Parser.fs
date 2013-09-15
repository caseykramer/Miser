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
        | [],rest -> Some(toString rest)
        | _ -> None
    loop (prefix,input)

let (|StringStartsWith|_|) prefix input = 
    match (asCharList input) with
    | StartsWith prefix (rest)-> Some rest
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

let rec (|WS|_|) (input:string) = 
    match asCharList input with
    | StartsWith [' '] (WS input)
    | StartsWith ['\t'] (WS input)
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
            comment |> (fun c -> Ast.Comment c,rest) |> Some
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
                | Some (body,rest) ->  Some (Ast.CommentBlock (List.rev ((body |> toString)::comment)),[rest])
        getComment (lines |> List.tail) [comment]
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
    | WS (StringStartsWith ['*']                 (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Any,ident),rest)
    | WS (StringStartsWith (asCharList "cpp")    (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace (Ast.NamespaceScope.Cpp,ident),rest)
    | WS (StringStartsWith (asCharList "java")   (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace (Ast.NamespaceScope.Java,ident),rest)
    | WS (StringStartsWith (asCharList "py" )    (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace (Ast.NamespaceScope.Py,ident),rest)
    | WS (StringStartsWith (asCharList "perl")   (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace (Ast.NamespaceScope.Perl,ident),rest)
    | WS (StringStartsWith (asCharList "rb")     (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace (Ast.NamespaceScope.Rb,ident),rest)
    | WS (StringStartsWith (asCharList "cocoa")  (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.Cocoa,ident),rest)
    | WS (StringStartsWith (asCharList "csharp") (WS (Identifier (ident,rest)))) -> Some(Ast.Namespace(Ast.NamespaceScope.CSharp,ident),rest)
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
    | StartsWith (asCharList "bool") rest   -> Some(Ast.BaseType.Bool,rest)
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
    | Identifier (ident,rest) -> Some (Ast.FieldType.IdentifierField ident,rest)
    | BaseType (baseType,rest) -> Some (Ast.FieldType.BaseField baseType,rest)
    | ContainerType (containerType,rest) -> Some (Ast.FieldType.ContainerField containerType,rest)
    | _ -> None
and (|ContainerType|_|) input = 
    match input with
    | StringStartsWith (asCharList "set") (FieldType (ftype,rest)) -> Some (Ast.ContainerType.Set(ftype),rest)
    | StringStartsWith (asCharList "list") (FieldType (ftype,rest)) -> Some (Ast.ContainerType.List(ftype),rest)
    | StringStartsWith (asCharList "map") (FieldType (ftype1,FieldType (ftype2,rest))) -> Some (Ast.ContainerType.Map(ftype1,ftype2),rest)
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

        