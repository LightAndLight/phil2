{-# language StandaloneDeriving, UndecidableInstances #-}
module Phil.Eval where

import Data.Map (Map)
import Data.Int (Int64)
import qualified Data.Map as Map

import Phil.Core (Expr(..), Quotable(..))

data Value ty ann
  = VClosure (Map String (Expr ty ann)) String (Expr ty ann)
  | VCtor String [Value ty ann]
  | VString String
  | VInt Int64
  | VApp (Value ty ann) (Value ty ann)
deriving instance (Show (ty String), Show ann) => Show (Value ty ann)

eval
  :: (Quotable ann, Quotable (ty String))
  => Map String (Expr ty ann)
  -> Expr ty ann
  -> Value ty ann
eval ctxt e =
  case e of
    Var _ name ->
      maybe (error "stuck in Var") (eval ctxt) $ Map.lookup name ctxt
    Abs _ n body -> VClosure ctxt n body
    App _ f x ->
      case eval ctxt f of
        -- call by value
        VClosure env name body -> eval (Map.insert name x env) body
        f' -> VApp f' (eval ctxt x)
    Hole _ -> error "stuck in hole"
    Quote _ e' -> eval ctxt (quote e')
    String _ s -> VString s
    Int _ i -> VInt i
    Unquote _ e' -> maybe (error "stuck in Unquote") (eval ctxt) (unquote e')
    Ann _ e' _ -> eval ctxt e'
