module Type where

import Data.List (intercalate)


data Type = TInt
          | TRef Type
          | TVoid
          | TArray Type (Maybe Integer)
          | TFun Type [Type]

instance Eq Type where
    TInt == TInt = True
    TInt == _ = False
    TRef t == TRef t' = t == t'
    TRef{} == _ = False
    TVoid == TVoid = True
    TVoid == _ = False
    TArray t _ == TArray t' _ = t == t'
    TArray{} == _ = False
    TFun ret args == TFun ret' args' = (ret,args) == (ret',args')
    TFun{} == _ = False

instance Show Type where
    show TInt = "int"
    show (TRef t) = show t ++ "*"
    show TVoid = "void"
    show (TArray t mn) = show t++"["++ns++"]"
        where ns = maybe "" show mn
    show (TFun ret args) = show ret ++ "("++intercalate ", " (fmap show args)++")"
