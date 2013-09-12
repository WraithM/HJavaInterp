module Ast where

data Program = Program [ClassDecl]

data ClassDecl = Class 
    { className     :: Id
    , classSuper    :: Id 
    , classVars     :: [(VarKind, VarDecl)] 
    , classMethods  :: [MethodDecl]
    } deriving (Show, Eq)

-- Check naming of fields
data MethodDecl = Method
    { retType       :: ExpType
    , methodName    :: Id
    , methodArgs    :: [VarDecl]
    , methodLocals  :: [VarDecl]
    , methodStmts   :: [Statement]
    , retExp        :: Exp
    } deriving (Show, Eq)

data VarKind = Static | NonStatic deriving (Show, Eq)
data VarDecl = Var 
    { varType :: ExpType 
    , varName :: Id
    } deriving (Show, Eq)

data Statement = Block [Statement]
    | If Exp Statement Statement
    | While Exp Statement
    | Println Exp
    | Assignment Id Exp
    | ArrayAssignment Id Exp Exp
    | Break
    | Continue
    deriving (Show, Eq)

data Exp = Operation Exp BinOp Exp
    | Subscript Exp Exp
    | Length Exp
    | MethodCall Exp Id [Exp]
    | FieldRef Exp Id
    | EInteger Int
    | ETrue | EFalse
    | EId Id
    | This
    | NewArray ExpType Exp
    | NewId Id
    | Not Exp
    | Null
    | EString String
    | EFloat Float
    deriving (Show, Eq)

data BinOp = And
    | Or
    | Equal
    | LessThan
    | LessThanEq
    | GreaterThan
    | GreaterThanEq
    | Plus
    | Minus
    | Multiplication
    | Division
    deriving (Show, Eq)

data ExpType = ArrayType ExpType
    | BoolType
    | IntType
    | ObjectType Id
    | StringType
    | FloatType
    deriving (Show, Eq)

type Id = String
