{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language TemplateHaskell #-}
module Phil.Core where

import Control.Lens.Lens (lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Plated1 (Plated1(..))
import Control.Lens.TH (makePrisms)
import Control.Monad (ap)
import Control.Monad.Unify (AsVar(..), HasAnnotation(..), Unifiable(..))
import Data.Deriving (deriveOrd1, deriveEq1, deriveShow1)
import GHC.Generics (Generic)

data Expr ann
  = Var String
  | Abs String (Expr ann)
  | App (Expr ann) (Expr ann)
  | Ann ann (Expr ann)
  deriving (Eq, Show, Generic)
instance Plated (Expr ann) where
  plate = gplate
makePrisms ''Expr

data Type ann a
  = TyVar a
  | TyArr (Type ann a) (Type ann a)
  | TyCtor String
  | TyAnn ann (Type ann a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data TypeScheme ann a = Forall [a] (Type ann a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveShow1 ''Type

instance Plated1 (Type ann) where; plate1 = gplate

makePrisms ''Type

instance AsVar (Type ann) where; _Var = _TyVar

instance HasAnnotation (Type ann) ann where
  annotation = lens (\(TyAnn a _) -> a) (\(TyAnn a b) a' -> TyAnn a' b)

instance Monad (Type ann) where
  return = TyVar
  TyVar a >>= f = f a
  TyArr a b >>= f = TyArr (a >>= f) (b >>= f)
  TyCtor a >>= _ = TyCtor a
  TyAnn a b >>= f = TyAnn a (b >>= f)

instance Applicative (Type ann) where; pure = return; (<*>) = ap

instance Unifiable (Type ann) where
  toplevelEqual TyArr{} TyArr{} = True
  toplevelEqual (TyAnn a b) c = toplevelEqual b c
  toplevelEqual a (TyAnn b c) = toplevelEqual a c
  toplevelEqual (TyCtor a) (TyCtor b) = a == b
  toplevelEqual _ _ = False
