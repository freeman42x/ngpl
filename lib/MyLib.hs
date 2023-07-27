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

substitute :: Term -> Term -> Term
substitute body arg = subst body
  where
    subst (Var x)          = if x == arg then arg else Var x
    subst (Lambda x t body) = if x == arg then Lambda x t body else Lambda x t (subst body)
    subst (App t1 t2)      = App (subst t1) (subst t2)
    subst (TAbs x k body)  = TAbs x k (subst body)
    subst (TApp t ty)      = TApp (subst t) ty

substituteType :: Term -> Type -> Term
substituteType body ty = subst body
  where
    subst (Var x)          = Var x
    subst (Lambda x t body) = Lambda x (substType t) (subst body)
    subst (App t1 t2)      = App (subst t1) (subst t2)
    subst (TAbs x k body)  = if x == ty then TAbs x k body else TAbs x k (subst body)
    subst (TApp t ty')     = TApp (subst t) (substType ty')

substType :: Type -> Type -> Type
substType body ty = subst body
  where
    subst (TVar x)            = if x == ty then ty else TVar x
    subst (TArr t1 t2)        = TArr (subst t1) (subst t2)
    subst (Forall x k t)      = if x == ty then Forall x k t else Forall x k (subst t)
    subst (TLambda x k t)     = if x == ty then TLambda x k t else TLambda x k (subst t)
    subst (TApp t1 t2)        = TApp (subst t1) (subst t2)

eval :: Term -> Term
eval (App (Lambda _ _ body) arg) = substitute body arg
eval (TApp (TAbs _ _ body) t)    = substituteType body t
eval term                        = term