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
  let sam3 = Let "f" (Lambda "n" (Add (Var "n") (Lit 5))) (Application (Var "f") (Lit 1))


-- represent Env using Maps
data Env = Env (Map Identifier Value)

env0 :: Env
env0 = Env (Map.fromList [("x",100)])

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


type Value = Int
type Identifier = String

eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> n
  Var x -> lookup env x
  Add l r -> eval env l + eval env r
  Let name rhs body -> eval (extend env name (eval env rhs)) body

data Exp
  = Lit Int
  | Var Identifier
  | Add Exp Exp
  | Let { name :: Identifier, rhs :: Exp, body :: Exp }
  | Lambda Identifier Exp
  | Application Exp Exp

instance Show Exp where
  show :: Exp -> String
  show = \case
    Lit n -> show n
    Var x -> x
    Add l r -> "(" ++ show l ++ "+" ++ show r ++ ")"
    Let _name _rhs _body -> "ebc todo"
