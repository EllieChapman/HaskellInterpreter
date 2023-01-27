module Top where

import Prelude hiding (lookup)

import qualified Data.Map as Map (fromList,lookup,insert)
import Data.Map (Map)


-- comment out labmda and appication
-- and built in that works on anothe rtype liek string, ie string_concat, and having literal strings
-- have program swhich can evaluate to a string and some to ints
main :: IO ()
main = do
  --let sam = Add (Lit 1) (Add (Lit 2) (Lit 3))
  let sam = Add (Lit 1) (Add (Var "x") (Lit 3))
  print sam
  print (eval env0 sam)
  let sam2 = Add (Let "x" (Lit 1) (Add (Var "x") (Var "x"))) (Var "x")
  print (eval env0 sam2)
  -- let sam3 = Let "f" (Lambda "n" (Add (Var "n") (Lit 5))) (Application (Var "f") (Lit 1))
  -- print (eval env0 sam3)
  let sam3 = Let "hw" (Concat (LitS "hello, ") (LitS "world")) (Concat (Var "hw") (LitS "!!"))
  print sam3
  print (eval env0 sam3)

  let sam4 = Let "double" (Lam "x" (Add (Var "x") (Var "x"))) (App (Var "double") (Lit 5))
  print sam4
  print (eval env0 sam4)


-- represent Env using Maps
data Env = Env (Map Identifier Value)

env0 :: Env
env0 = Env (Map.fromList [("x", VI 100)])

extend :: Env -> Identifier -> Value -> Env
extend (Env m) id value =
    Env (Map.insert id value m)

lookup :: Env -> Identifier -> Value
lookup (Env m) x =
  case Map.lookup x m of
    Just v -> v
    Nothing -> error "sdfkjhsdkj"


{-
-- represent Env using functions
data Env = EnvCC (Identifier -> Value)

env0 :: Env
env0 = EnvCC (\x -> case x of
                 "x" -> 100
                 "y" -> 200
                 _ -> 42)

lookup :: Env -> Identifier -> Value
lookup (EnvCC f) x = f x
-}


data Value = VI Int | VS String | VF deriving (Show)
type Identifier = String

eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> VI n
  LitS s -> VS s
  Var x -> lookup env x
  Add l r -> addV (eval env l) (eval env r)
  Concat l r -> concatV (eval env l) (eval env r)
  Let name rhs body -> eval (extend env name (eval env rhs)) body
  App l r -> apply (eval env l) (eval env r)
  Lam {} -> undefined -- ???? will need a way to create bidnigns in the env

apply :: Value -> Value -> Value
apply = undefined

addV :: Value -> Value -> Value
addV v1 v2 = VI (getI v1 + getI v2)

getI :: Value -> Int
getI = \case
  VI i -> i
  v -> error (show ("getI failed", v))

concatV :: Value -> Value -> Value
concatV v1 v2 = VS (getS v1 ++ getS v2)

getS :: Value -> String
getS = \case
  VS s -> s
  v -> error (show ("getS failed", v))

data Exp
  = Lit Int
  | LitS String
  | Var Identifier
  | Add Exp Exp
  | Concat Exp Exp
  | Let { name :: Identifier, rhs :: Exp, body :: Exp }
  | Lam Identifier Exp
  | App Exp Exp

instance Show Exp where
  show :: Exp -> String
  show = \case
    Lit n -> show n
    LitS s -> show s
    Var x -> x
    Add l r -> "(" ++ show l ++ "+" ++ show r ++ ")"
    Concat l r -> "(" ++ show l ++ " ++ " ++ show r ++ ")"
    Let name rhs body -> "(let " ++ name ++ " = " ++ show rhs ++ " in " ++ show body ++ ")"
    App l r -> "(" ++ show l ++ " " ++ show r ++ ")"
    Lam name body -> "(\\" ++ name ++ " -> " ++ show body ++ ")"

-- instance Show Value where
--   show :: Value -> String
--   show = \case
--     VI i -> show i
--     VS s -> show s
