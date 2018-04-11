module Phil.Builtins where

import Phil.Core (Type(..))

tyExpr :: Type ann a
tyExpr = TyCtor Nothing "Expr"

tyString :: Type ann a
tyString = TyCtor Nothing "String"

tyInt :: Type ann a
tyInt = TyCtor Nothing "Int"
