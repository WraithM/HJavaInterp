data Program = Program [ClassDecl]

data ClassDecl = Class 
    { name  :: Id 
    , super :: Id 
    , vars  :: [(VarKind, VarDecl)] 
    , methods :: [MethodDecl]
    } deriving (Show, Eq)

data VarDecl = Var ExpType Id

data VarKind = Static | NonStatic

data Statement = Block [Statement]
    | If Exp Statement Statment
    | While Exp Statement
    | Println Exp
    | Assignment Id Exp
    | ArrayAssignment Id Exp Exp
    | Break
    | Continue

data Exp = Operation Exp BinOp Exp
    | Subscript Exp Exp
    | Length Exp
    | MethodCall Exp Id [Exp]
    | FieldRef Exp Id
    | Integer Int
    | ETrue | EFalse
    | EId Id
    | This
    | NewArray ExpType Exp
    | NewId Id
    | Not Exp
    | Null
    | EString String
    | EFloat Float

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

data ExpType = ArrayType ExpType
    | BoolType
    | IntType
    | ObjectType Id
    | StringType
    | FloatType

type Id = String



