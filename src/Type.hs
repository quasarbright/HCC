module Type where

data Type = TInt | TRef Type | TVoid deriving(Eq, Ord)

instance Show Type where
    show TInt = "int"
    show (TRef t) = show t ++ "*"
    show TVoid = "void"