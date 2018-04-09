{-# language StandaloneDeriving, UndecidableInstances #-}
module Phil.Eval where

import Data.Map (Map)
import Data.Int (Int64)
import qualified Data.Map as Map

import Phil.Core (Expr(..), quoteExpr, unquoteExpr)

data Value ty ann
  = VClosure (Map String (Expr ty ann)) String (Expr ty ann)
  | VCtor String [Value ty ann]
  | VString String
  | VInt Int64
  | VApp (Value ty ann) (Value ty ann)
deriving instance (Show (ty String), Show ann) => Show (Value ty ann)

data Quoting ty ann
  = Quoting
  { qAnn :: ann -> Expr ty ann
  , uAnn :: Expr ty ann -> Maybe ann
  , qTy :: ty String -> Expr ty ann
  , uTy :: Expr ty ann -> Maybe (ty String)
  }

eval
  :: Quoting ty ann
  -> Map String (Expr ty ann)
  -> Expr ty ann
  -> Value ty ann
eval quoting ctxt e =
  case e of
    Var _ name ->
      maybe (error "stuck in Var") (eval quoting ctxt) $ Map.lookup name ctxt
    Abs _ n body -> VClosure ctxt n body
    App _ f x ->
      case eval quoting ctxt f of
        -- call by value
        VClosure env name body -> eval quoting (Map.insert name x env) body
        f' -> VApp f' (eval quoting ctxt x)
    Hole _ -> error "stuck in hole"
    Quote _ e' ->
      eval quoting ctxt (quoteExpr (qTy quoting) (qAnn quoting) e')
    String _ s -> VString s
    Int _ i -> VInt i
    Unquote _ e' ->
      maybe
        (error "stuck in Unquote")
        (eval quoting ctxt)
        (unquoteExpr (uTy quoting) (uAnn quoting) e')
    Ann _ e' _ -> eval quoting ctxt e'
