module ThriftAST

type Identifier = Identifier of string

type Literal = StringLiteral of string
type Constant = 
    | IntegerConstant of int64
    | DecimalConstant of float
    | ListConstant of Constant list
    | MapConstant of (Constant*Constant) list
    | IdentifierConstant of Identifier
    | LiteralConstant of Literal

[<RequireQualifiedAccess>]
type BaseType = 
    | Bool
    | Byte 
    | I16
    | I32
    | I64
    | Double
    | String
    | Binary
    | SList

type FieldType = 
    | IdentifierField of Identifier
    | BaseField of BaseType
    | ContainerField of ContainerType
and ContainerType = 
    | Map of FieldType*FieldType
    | List of FieldType
    | Set of FieldType
type DefinitionType = 
    | BaseDefinition of BaseType
    | ContainerDefinition of ContainerType

type FieldDefinition = 
    | Field of fieldId:int64 option*isReq:bool option*fieldType:FieldType*identifier:Identifier*value:Constant option

type Throws = 
    | Throws of FieldDefinition list

type FunctionType = 
    | Void
    | Type of FieldType

type Function = Function of isOneWay:bool option*functiontype:FunctionType*identifier:Identifier*FieldDefinition list*throws:Throws option

type ServiceDefinition = Service of identifier:Identifier*extends:Identifier option*functions:Function list

type ExceptionDefinition = Exception of identifier:Identifier*fields:FieldDefinition list

type UnionDefinition = Union of identifier:Identifier*fields:FieldDefinition list

type StructDefinition = Struct of identifier:Identifier*fields:FieldDefinition list

type EnumDefinition = Enum of identifier:Identifier*values:(Identifier*int64 option) list

type TypeDef = TypeDef of defType:DefinitionType*name:Identifier

type ConstDefinition = Const of constType:FieldType*identifier:Identifier*value:Constant

type DefinitionInfo = 
    | ConstDef of ConstDefinition
    | TypeDef of TypeDef
    | EnumDef of EnumDefinition
    | StructDef of StructDefinition
    | UnionDef of UnionDefinition
    | ExceptionDef of ExceptionDefinition
    | ServiceDef of ServiceDefinition

type NamespaceScope = 
    | All
    | Cpp
    | Java
    | Py
    | Perl
    | Ruby
    | Cocoa
    | CSharp
    | FSharp
    | Php
    | Smalltalk
    | Other of string

type Namespace = Namespace of scope:NamespaceScope*identifier:Identifier

type Include = Include of Literal

type HeaderInfo =
    | IncludeHeader of Include
    | NamespaceHeader of Namespace
    
type Document = Document of HeaderInfo list*DefinitionInfo list

type Headers = Headers
type Structs = Structs
type Services = Services

type Source = File of DefinitionInfo list
type Project = Project of Source list