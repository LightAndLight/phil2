{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
module Phil.Core where

import Control.Lens.Lens (lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Plated1 (Plated1(..))
import Control.Lens.Prism (prism')
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
  = TyVar (Maybe ann) a
  | TyArr (Maybe ann) (Type ann a) (Type ann a)
  | TyCtor (Maybe ann) String
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data TypeScheme ann a = Forall [a] (Type ann a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveShow1 ''Type

instance Plated1 (Type ann) where; plate1 = gplate

makePrisms ''Type

instance AsVar (Type ann) where
  _Var =
    prism'
      (TyVar Nothing)
      (\case
          TyVar _ a -> Just a
          _ -> Nothing)

instance HasAnnotation (Type ann) (Maybe ann) where
  annotation =
    lens
      (\case
         TyVar a _ -> a
         TyArr a _ _ -> a
         TyCtor a _ -> a)
      (\e a' ->
         case e of
           TyVar _ b -> TyVar a' b
           TyArr _ b c -> TyArr a' b c
           TyCtor _ b -> TyCtor a' b)

instance Monad (Type ann) where
  return = TyVar Nothing
  TyVar _ a >>= f = f a
  TyArr ann a b >>= f = TyArr ann (a >>= f) (b >>= f)
  TyCtor ann a >>= _ = TyCtor ann a

instance Applicative (Type ann) where; pure = return; (<*>) = ap

instance Unifiable (Type ann) where
  toplevelEqual TyArr{} TyArr{} = True
  toplevelEqual (TyCtor _ a) (TyCtor _ b) = a == b
  toplevelEqual _ _ = False
