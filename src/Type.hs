module Type where

data Type = TInt | TRef Type | TVoid | TArray Type (Maybe Integer)

instance Eq Type where
    TInt == TInt = True
    TInt == _ = False
    TRef t == TRef t' = t == t'
    TRef{} == _ = False
    TVoid == TVoid = True
    TVoid == _ = False
    TArray t _ == TArray t' _ = t == t'
    TArray{} == _ = False

instance Show Type where
    show TInt = "int"
    show (TRef t) = show t ++ "*"
    show TVoid = "void"
    show (TArray t mn) = show t++"["++ns++"]"
        where ns = maybe "" show mn
