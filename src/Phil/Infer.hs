module Phil.Infer where

import Control.Monad.Unify (UnifyT)
import Data.Map (Map)

import Phil.Core (Expr(..), Type(..))

data TypeError tyAnn ann v
  = NotFound (Maybe ann) String

infer :: Map String (Type tyAnn v) -> Expr ann -> Type tyAnn v
infer (Ann ann expr) = go (Just ann) expr
infer expr = go Nothing expr
  where
    go maybeAnn expr =
      case expr of
        Ann ann expr' -> go (Just ann) expr'
        Var v -> _
        Abs n expr' -> _
        App f x -> _
