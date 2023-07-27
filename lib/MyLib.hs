module MyLib (someFunc) where

import Data.List (find)

data Kind = Star | KArr Kind Kind deriving (Show, Eq)

data Type = TVar String 
          | TArr Type Type 
          | Forall String Kind Type 
          | TLambda String Kind Type
          | TApp Type Type deriving (Show, Eq)

data Term = Var String 
          | Lambda String Type Term 
          | App Term Term 
          | TAbs String Kind Term 
          | TApp Term Type deriving (Show, Eq)

data Binding = VarBind String Type | TyVarBind String Kind deriving (Show, Eq)

type Context = [Binding]

getTypeFromContext :: Context -> String -> Maybe Type
getTypeFromContext context var = case find isVarBindWithName context of
    Just (VarBind _ t) -> Just t
    _                  -> Nothing
  where
    isVarBindWithName (VarBind name _) = name == var
    isVarBindWithName _                = False

getKindFromContext :: Context -> String -> Maybe Kind
getKindFromContext context var = case find isTyVarBindWithName context of
    Just (TyVarBind _ k) -> Just k
    _                    -> Nothing
  where
    isTyVarBindWithName (TyVarBind name _) = name == var
    isTyVarBindWithName _                  = False

eval :: Term -> Term
eval (App (Lambda _ _ body) arg) = substitute body arg
eval (TApp (TAbs _ _ body) t)    = substituteType body t
eval term                        = term