module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal a) (BoolVal b) = BoolVal $ op a b
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal a) (IntVal b) = BoolVal $ op a b
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env =
    case H.lookup s env of
        Nothing -> ExnVal "No match in env"
        Just v -> v

--- ### Arithmetic

eval (IntOpExp op e1 e2) env | (op == "/") && (eval e2 env == IntVal 0) = ExnVal "Division by 0"
                             | otherwise =
                                let
                                    Just v = H.lookup op intOps
                                in
                                    liftIntOp v (eval e1 env) (eval e2 env)

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = let
                                    Just v = H.lookup op boolOps
                                in
                                    liftBoolOp v (eval e1 env) (eval e2 env)

eval (CompOpExp op e1 e2) env = let
                                    Just v = H.lookup op compOps
                                in
                                    liftCompOp v (eval e1 env) (eval e2 env)

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
                        case eval e1 env of
                            BoolVal True -> eval e2 env
                            BoolVal False -> eval e3 env
                            _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    case eval e1 env of
        CloVal params body env1 -> 
            -- params: list of parameters, body: actual function body, env: environment
            -- need to get the params into the body
            -- params: [strings], body: exp
            -- 1. zip the args into the params
            -- 2. add to env1
            -- 3. eval the body with
            let newArgs = H.fromList (zip params (map (\a -> eval a env) args)) in
                let newEnv = H.union newArgs env1 in
                    eval body newEnv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = 
    -- take in a pairs [(variable, value)], expression on variable
    -- add the result of the expression to the environment
    -- put the value into corresponding place in the body and (eval body newenv)
    let (vars, val) = unzip pairs in
        let res = H.fromList (zip vars (map (\a -> eval a env) val)) in
            let newEnv = H.union res env in
                eval body newEnv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = 
    let newVal = eval e env in
        let newEnv = H.insert var newVal env in 
            ("", penv, newEnv)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = 
    let (res1, penv1, env1) = exec x penv env in
        let (rest, penvr, envr) = exec (SeqStmt xs) penv1 env1 in 
            (res1 ++ rest, penvr, envr)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = 
    -- need to add the stmt into the penv
    -- add a stmt as the value into penv with key name
    let newPenv = H.insert name p penv in
        ("", newPenv, env)

exec (CallStmt name args) penv env = 
    -- same as appExp
    -- evaluate the args
    -- update the parameters and environments from the penv lookup
    -- exec the body of the function on the new environment
    case H.lookup name penv of
        Nothing -> ("", penv, env)
        Just (ProcedureStmt name params body) -> 
                let res = H.fromList (zip params (map (\a -> eval a env) args)) in
                    let newEnv = H.union res env in
                        exec body penv newEnv