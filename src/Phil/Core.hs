{-# language DeriveFunctor, DeriveGeneric #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
module Phil.Core where

import GHC.Generics (Generic)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Plated1 (Plated1(..))
import Control.Lens.TH (makePrisms)
import Control.Monad.Unify (AsVar(..), HasAnnotation(..))

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
  deriving (Eq, Show, Generic)
instance Plated1 (Type ann) where
  plate1 = gplate
makePrisms ''Type
instance AsVar (Type ann) where
  _Var = _TyVar
instance HasAnnotation (Type ann) ()
