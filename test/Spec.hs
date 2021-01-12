
import Test.Hspec

import Type
import Typing
import Parse
import qualified WellFormedness as WF
import AST

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

wfStringExpr :: String -> [WF.WFError]
wfStringExpr src =
    case parseExpr "src/Spec.hs" src of
            Left err -> [WF.InternalError err] -- tuck parse errors in here
            Right e -> WF.executeChecker [] (WF.checkExpr e)

wfStringProg :: String -> [WF.WFError]
wfStringProg src =
    case parseProgram "src/Spec.hs" src of
            Left err -> [WF.InternalError err] -- tuck parse errors in here
            Right prog -> WF.executeChecker [] (WF.checkProgram prog)

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
        it "checks empty array literals" $ do
            inferStringProg "int[] xs = {};" `shouldBe` Right TVoid
        it "doesn't let then and else have different non-void types" $ do
            inferStringProg "if(1) { return 1; } else { int *y; return y; }" `shouldBe` Left (Mismatch TInt (TRef TInt))
        it "ensures if/loop condition is an int" $ do
            inferStringProg "int *y; if(y) {}" `shouldBe` Left (Mismatch TInt (TRef TInt))
            inferStringProg "int *y; while(y) {}" `shouldBe` Left (Mismatch TInt (TRef TInt))
            inferStringProg "int *y; for(int x;y; x = 1;) {}" `shouldBe` Left (Mismatch TInt (TRef TInt))
        it "allows valid ifs" $ do
            -- TODO remove extra return after you fix statement checking
            inferStringProg "if(1) { return 1; } else { return 2; } return 3;" `shouldBe` Right TInt
        it "detects errors in if branches" $ do
            inferStringProg "if(1) { return *1; }" `shouldBe` Left (BadDeref TInt)
            inferStringProg "if(1) {} else { return *1; }" `shouldBe` Left (BadDeref TInt)
    describe "well formedness check" $ do
        it "detects unbound variables" $ do
            wfStringExpr "x" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "1 + (2 * x)" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "1 + (2 * x)" `shouldBe` [WF.UnboundVar "x"]
            wfStringProg "int x; return 2 + x + y;" `shouldBe` [WF.UnboundVar "y"]
            wfStringExpr "x + x + y" `shouldBe` [WF.UnboundVar "x", WF.UnboundVar "x", WF.UnboundVar "y"]
            wfStringProg "int x = y; return 2 + x + y;" `shouldBe` [WF.UnboundVar "y", WF.UnboundVar "y"]
            wfStringProg "int x = x;" `shouldBe` [WF.UnboundVar "x"]
            wfStringProg "int[] xs = {1,2}; return xs;" `shouldBe` [] -- regression test
            wfStringProg "x = 1;" `shouldBe` [WF.UnboundVar "x"]
        it "detects duplicate variables" $ do
            wfStringProg "int x; int x;" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int x; int x = 1;" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int x = 1; int x = 1;" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int x = 1; int x;" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int x = 1; int y; int x;" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int x; int x; int y; int x; int y;" `shouldBe` (WF.DupVar <$> ["x", "x", "y"])
        it "knows assignments don't declare new vars" $ do
            wfStringProg "int x = 1; x = 2;" `shouldBe` []
        it "knows where array literals belong where they don't" $ do
            wfStringProg "int[] x = {1,2,3};" `shouldBe` []
            wfStringProg "int[] x; x = {1,2,3};" `shouldBe` [WF.UnexpectedArrayLiteral (EArrayLiteral (EInt <$> [1,2,3]))]
        it "detects bad addr of" $ do
            wfStringExpr "&1" `shouldBe` [WF.BadAddrOf (EInt 1)]
            let e = EArrayLiteral [EInt 1, EInt 2]
                in wfStringExpr "&{1,2}" `shouldBe` [WF.BadAddrOf e, WF.UnexpectedArrayLiteral e]
            wfStringExpr "&(1 + 2)" `shouldBe` [WF.BadAddrOf (EBinop Plus (EInt 1) (EInt 2))]
            wfStringExpr "&(1 + x)" `shouldBe` [WF.BadAddrOf (EBinop Plus (EInt 1) (EVar "x")), WF.UnboundVar "x"]
            wfStringExpr "&x" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "&*x" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "&(xs[0])" `shouldBe` [WF.UnboundVar "xs"]
            wfStringExpr "&*(xs[0])" `shouldBe` [WF.UnboundVar "xs"]
        it "detects negative index errors" $ do
            wfStringProg "int[] xs; return xs[-1];" `shouldBe` [WF.NegativeIndex (EGetIndex (EVar "xs") (EUnop Neg (EInt 1)))]
            wfStringProg "int[] xs; return xs[0];" `shouldBe` []
            wfStringProg "int[] xs; return xs[0 + (-1)];" `shouldBe` []
        it "handles ifs" $ do
            wfStringProg "if (x) { int x; return x + y; } else { return x; }" `shouldBe` (WF.UnboundVar <$> ["x","y","x"])
        it "has proper scoping for loop variables" $ do
            wfStringProg "for(int x = 0; 1; x = x;) {} return x;" `shouldBe` [WF.UnboundVar "x"]
        it "detects errors in for parens" $ do
            wfStringProg "for(int x = x; y; x = z;) {}"  `shouldBe` WF.UnboundVar <$> ["x","y","z"]
        it "detects errors in loop bodies" $ do
            wfStringProg "for(int x =1; x; x = 1;) {int a = b; return z;}" `shouldBe` WF.UnboundVar <$> ["b","z"]
            wfStringProg "while(1) {int a = b; return z;}" `shouldBe` WF.UnboundVar <$> ["b","z"]
        it "allows shadowing" $ do
            wfStringProg "int x; if(1) { int x; }" `shouldBe` []
{-
TODO test int f(int f) scope precedence
TODO test int f(int x, int x); 
TODO test dup fun def, dup fun decl, int f(); int f(){}  
-}