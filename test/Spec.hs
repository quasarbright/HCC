
import Test.Hspec

import Type
import Typing
import Parse

inferStringExpr :: String -> Either TypeError Type
inferStringExpr src =
    case parseExpr "src/Spec.hs" src of
        Left err -> Left (InternalError err) -- tuck parse errors in here
        Right e -> executeChecker mempty (inferExpr e)

inferStringProg :: String -> Either TypeError Type
inferStringProg src =
    case parseProgram "src/Spec.hs" src of
        Left err -> Left (InternalError err)
        Right prog -> executeChecker mempty (inferProgram prog)

main :: IO ()
main = hspec $ do
    describe "type checker" $ do
        it "infers ints" $ do
            inferStringExpr "1" `shouldBe` Right TInt
        it "infers arithmetic unops" $ do
            inferStringExpr "-(1)" `shouldBe` Right TInt
            inferStringExpr "~(1)" `shouldBe` Right TInt
            inferStringExpr "!(1)" `shouldBe` Right TInt
            inferStringProg "int x; return -(&x);" `shouldBe` Left (Mismatch TInt (TRef TInt))
        it "infers deref and addr of" $ do
            inferStringProg "int x; int *y = &x; return y;" `shouldBe` Right (TRef TInt)
            inferStringProg "int x; int *y = &x; return *y;" `shouldBe` Right TInt
            inferStringProg "int x; int *y = &x; int **z = &y; return z;" `shouldBe` Right (TRef (TRef TInt))
            inferStringProg "int x; int *y = &x; int **z = &y; return *z;" `shouldBe` Right (TRef TInt)
            inferStringProg "int x; int *y = &x; int **z = &y; return **z;" `shouldBe` Right TInt
            inferStringProg "int x; int *y = &x; int **z = &y; int a; *z = &a; **z = x;" `shouldBe` Right TVoid
        it "handles relaxed addr of" $ do
            inferStringProg "int x; int *y = &x; return &*y;" `shouldBe` Right (TRef TInt)
            inferStringProg "int x; int *y = &x; int **z = &y; return &*z;" `shouldBe` Right (TRef (TRef TInt))
            inferStringProg "int x; int *y = &x; int **z = &y; return &**z;" `shouldBe` Right (TRef TInt)
        it "doesn't let you dereference an int" $ do
            inferStringProg "int x; return *x;" `shouldBe` Left (BadDeref TInt)
            inferStringProg "int x; int *y = &x; return **y;" `shouldBe` Left (BadDeref TInt)
--        it "doesn't allow bad addrOfs"
        it "infers arithmetic binops" $ do
            inferStringExpr "1 + 1" `shouldBe` Right TInt
            inferStringExpr "1 * 1" `shouldBe` Right TInt
            inferStringExpr "1 & 1" `shouldBe` Right TInt
            inferStringExpr "1 | 1" `shouldBe` Right TInt
            inferStringExpr "1 + 2 * 3" `shouldBe` Right TInt
        it "infers equality" $ do
            inferStringExpr "1 == 1" `shouldBe` Right TInt
            inferStringProg "int x; return &x == &x;" `shouldBe` Right TInt
        it "doesn't let you do equality on different types" $ do
            inferStringProg "int x; return &x == x;" `shouldBe` Left (Mismatch (TRef TInt) TInt)
        it "handles assignments" $ do
            inferStringProg "int x; int y; x = y;" `shouldBe` Right TVoid
            inferStringProg "int *x; int *y; x = y;" `shouldBe` Right TVoid
            inferStringProg "int *x; int y; x = y;" `shouldBe` Left (Mismatch (TRef TInt) TInt)
            inferStringProg "int *x; int y; y = x;" `shouldBe` Left (Mismatch TInt (TRef TInt))
        it "handles dereference assignment" $ do
            inferStringProg "int x; int *y = &x; *y = 2;" `shouldBe` Right TVoid
            inferStringProg "int x; int *y = &x; int **z = &y; *z = &x; **z = 2;" `shouldBe` Right TVoid
            inferStringProg "int x; *x = 2;" `shouldBe` Left (BadDeref TInt)
            inferStringProg "int x; int *y = &x; **y = 2;" `shouldBe` Left (BadDeref TInt)
            inferStringProg "int x; int *y = &x; *y = y;" `shouldBe` Left (Mismatch TInt (TRef TInt))
            inferStringProg "int x; int *y = &x; int **z = &y; *z = x;" `shouldBe` Left (Mismatch (TRef TInt) TInt)
        it "handles arithmetic with dereferences" $ do
            inferStringProg "int x; int *y; int **z; return 1 + (*y + x + **z) + *&x;" `shouldBe` Right TInt
        it "handles arrays" $ do
            inferStringProg "int[] xs; return xs;" `shouldBe` Right (TArray TInt Nothing)
            inferStringProg "int[] xs; return xs[0];" `shouldBe` Right TInt
            inferStringProg "int[] xs = {1,2,3}; return xs[0];" `shouldBe` Right TInt
            inferStringProg "int x; int *y; int*[] xs = {y,y,&x}; return xs[0];" `shouldBe` Right (TRef TInt)
            inferStringProg "int x; int *y; int*[] xs = {y,y,&x}; return *xs[0];" `shouldBe` Right TInt
            
