module Phil.Printer where

import Data.Semigroup ((<>))

import Phil.Core (Expr(..), Type(..))

brackets :: String -> String
brackets a = "(" <> a <> ")"

printType :: (a -> String) -> Type ann a -> String
printType printA ty =
  case ty of
    TyVar _ a -> printA a
    TyArr _ a b -> nested a <> " -> " <> printType printA b
    TyCtor _ a -> a
  where
    nested a@TyArr{} = brackets (printType printA a)
    nested a = printType printA a

printExpr :: (ty String -> String) -> Expr ty a -> String
printExpr printTy e =
  case e of
    Var _ name -> name
    Abs _ n body -> "\\" <> n <> " -> " <> printExpr printTy body
    App _ f x -> printExpr printTy f <> " " <> nested x
    Hole _ -> "??"
    Quote _ e' -> "'" <> nested e'
    String _ s -> show s
    Int _ i -> show i
    Unquote _ e' -> "$" <> nested e'
    Ann _ e' ty ->
      (case e' of; Ann{} -> brackets; _ -> id) (printExpr printTy e') <>
      " : " <>
      printTy ty
  where
    nested a@App{} = brackets (printExpr printTy a)
    nested a@Ann{} = brackets (printExpr printTy a)
    nested a = printExpr printTy a
