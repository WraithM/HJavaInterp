import Ast
import Common

import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List (mapAccumL)
import Data.Tuple (swap)

assign :: Id -> StackValue -> Environment -> Environment
assign id _ [] = error $ "Type Error: Unbound variable " ++ id
assign id v ((id', v'):xs)
    | id == id' = (id, v) : xs
    | otherwise = (id', v') : assign id v xs

binds :: Id -> Environment -> Bool
binds id [] = False
binds id ((id', v):xs) = id == id' || binds id xs

fetch :: Id -> Environment -> StackValue
fetch id env = fromMaybe (error errorMsg) (lookup id env)
  where errorMsg = "Type Error: Unbound variable " ++ id

zipScalar :: [Id] -> StackValue -> Environment
zipScalar xs v = zip xs (replicate (length xs) v)

varNames :: [VarDecl] -> [Id]
varNames [] = []
varNames ((Var _ s):xs) = s : varNames xs

getMethodInClass :: Id -> ClassDecl -> MethodDecl
getMethodInClass id (Class _ _ _ ms) = 
    fromMaybe (error errorMsg) (lookup id (map (\m -> (methodName m, m)) ms))
      where errorMsg = "Type Error: No such method " ++ id

extend :: Store -> HeapValue -> Store
extend st hval = st ++ [hval]

storeFetch :: Store -> Int -> HeapValue
storeFetch st loc = st !! loc

assignField :: HeapValue -> VarName -> StackValue -> HeapValue
assignField (Object c flds) id sv = Object c (assign id sv flds)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 x (_:xs) = x:xs
replaceNth n y (x:xs) = x:replaceNth (n - 1) y xs

assignStore :: Store -> Int -> HeapValue -> Store
assignStore sto loc obj = replaceNth loc obj sto

getClass :: Id -> Program -> ClassDecl
getClass c (Program cs) = 
    fromMaybe (error errorMsg) (lookup c (map (\c -> (className c, c)) cs))
      where errorMsg = "No such class " ++ c

getMethod :: Id -> Id -> Program -> MethodDecl
getMethod id c prog
    | id `elem` mthdnames = getMethodInClass id cl
    | superid /= "" = getMethod id superid prog
    | otherwise = error $ "Type Error: Unbound method " ++ id
  where
    cl = getClass c prog
    Class _ superid _ mthds = cl
    mthdnames = map methodName mthds

fields :: Id -> Program -> [String]
fields c prog
    | superid == "" = fldnames
    | otherwise = fldnames ++ fields superid prog
  where
    cl = getClass c prog
    Class _ superid flds _ = cl
    fldnames = map (varName . snd) flds

lshow = map toLower . show

applyOp :: BinOp -> StackValue -> StackValue -> StackValue
applyOp Plus (IntV i) (IntV j) = IntV (i + j)
applyOp Plus (StringV i) (StringV j) = StringV (i ++ j)
applyOp Plus (StringV i) (IntV j) = StringV (i ++ show j)
applyOp Plus (IntV i) (StringV j) = StringV (show i ++ j)
applyOp Plus (StringV i) (BoolV j) = StringV (i ++ lshow j) 
applyOp Plus (BoolV i) (StringV j) = StringV (lshow i ++ j)
applyOp Minus (IntV i) (IntV j) = IntV (i - j)
applyOp Multiplication (IntV i) (IntV j) = IntV (i * j)
applyOp Division _ (IntV 0) = error "Runtime Error: Division by zero!"
applyOp Division (IntV i) (IntV j) = IntV (i `div` j)
applyOp Equal i j = BoolV (i == j)
applyOp And (BoolV i) (BoolV j) = BoolV (i && j)
applyOp Or (BoolV i) (BoolV j) = BoolV (i || j)
applyOp LessThan (IntV i) (IntV j) = BoolV (i < j)
applyOp LessThanEq (IntV i) (IntV j) = BoolV (i <= j)
applyOp GreaterThan (IntV i) (IntV j) = BoolV (i > j)
applyOp GreaterThanEq (IntV i) (IntV j) = BoolV (i >= j)
applyOp _ _ _ = error "Type Error: Failed to convert args"
    

-- Reader + Writer Monad, writer for store, reader for env
eval :: Exp -> State -> Program -> (StackValue, Store)
eval e (sigma@(env, sto)) prog = case e of
    EInteger i -> (IntV i, sto)
    EString s -> (StringV s, sto)
    ETrue -> (BoolV True, sto)
    EFalse -> (BoolV False, sto)
    Null -> (NullV, sto)
    Not e' -> case eval e' sigma prog of
        (BoolV b, sto') -> (BoolV (not b), sto')
        _ -> error "Type Error: Not requires boolean."
    Operation e1 And e2 -> case eval e1 sigma prog of
        (BoolV True, sto') -> eval e2 (env, sto') prog
        (BoolV False, sto') -> (BoolV False, sto')
        _ -> error "Type Error: And applied to non-boolean."
    Operation e1 Or e2 -> case eval e1 sigma prog of
        (BoolV False, sto') -> eval e2 (env, sto') prog
        (BoolV True, sto') -> (BoolV True, sto')
        _ -> error "Type Error: Or applied to non-boolean."
    Operation e1 bop e2 ->
        let (v1, sto') = eval e1 sigma prog
            (v2, sto'') = eval e2 (env, sto') prog
        in (applyOp bop v1 v2, sto'')
    EId id -> if binds id env then (fetch id env, sto) else
        let Location l = fetch "this" env
            Object _ env' = storeFetch sto l
        in (fetch id env', sto)
    This -> (fetch "this" env, sto)
    MethodCall e id exps ->
        evalMethodCall stmts retexp (env'', sto'') prog
          where
        (Location l, sto') = eval e sigma prog
        (stackvs, sto'') = evalList exps (env, sto') prog
        Object cid env' = storeFetch sto'' l
        Method _ methid args locals stmts retexp =
            getMethod id cid prog
        argsenv = zip (varNames args) stackvs
        localenv = zipScalar (varNames locals) NullV
        env'' = ("this", Location l) : argsenv ++ localenv
    NewId c -> (Location (length sto), sto')
        where
          env' = zipScalar (fields c prog) NullV
          sto' = extend sto (Object c env')
    _ -> error "Not Implemented in eval"

evalList :: [Exp] -> State -> Program -> ([StackValue], Store)
evalList el (env, sto) prog = swap $ mapAccumL (\sto' e -> swap $ eval e (env, sto') prog) sto el

evalMethodCall :: [Statement] -> Exp -> State -> Program -> (StackValue, Store)
evalMethodCall stms retval sigma prog = eval retval sigma' prog
  where sigma' = execStmts stms sigma prog

execStmt :: Statement -> State -> Program -> State
execStmt s (sigma@(env,sto)) prog = case s of
    Block stmts -> execStmts stmts sigma prog
    If e s1 s2 -> case eval e sigma prog of
        (BoolV True, sto') -> execStmt s1 (env, sto') prog
        (BoolV False, sto') -> execStmt s2 (env, sto') prog
        _ -> error "Type Error: If doesn't have a boolean expression."
    Assignment id e -> 
        if binds id env then (assign id v env, sto') else (env, assignStore sto' l obj) 
          where 
        (v, sto') = eval e sigma prog
        Location l = fetch "this" env
        obj = assignField (storeFetch sto' l) id v
    _ -> error "Not implemented execStmt"

execStmts :: [Statement] -> State -> Program -> State
execStmts ss sigma prog = foldl (\sigma' s -> execStmt s sigma' prog) sigma ss
