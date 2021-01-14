
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

typeStringBlock :: String -> Maybe TypeError
typeStringBlock src_ =
    let src = "void main(){"++src_++"}" in
    case parseProgram "src/Spec.hs" src of
        Left err -> Just (InternalError err)
        Right prog -> case typeCheckProgram prog of
                Left err -> Just err
                Right () -> Nothing

typeStringProg :: String -> Maybe TypeError
typeStringProg src =
    case parseProgram "src/Spec.hs" src of
        Left err -> Just (InternalError err)
        Right prog -> case typeCheckProgram prog of
                Left err -> Just err
                Right () -> Nothing

wfStringExpr :: String -> [WF.WFError]
wfStringExpr src =
    case parseExpr "src/Spec.hs" src of
            Left err -> [WF.InternalError err] -- tuck parse errors in here
            Right e -> WF.executeChecker [] (WF.checkExpr e)

wfStringBlock :: String -> [WF.WFError]
wfStringBlock src_ =
    let src = "int main(){"++src_++"}" in
    case parseProgram "src/Spec.hs" src of
            Left err -> [WF.InternalError err] -- tuck parse errors in here
            Right prog -> WF.executeChecker [] (WF.checkProgram prog)

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
            typeStringBlock "int x; return -(&x);" `shouldBe` Just (Mismatch TInt (TRef TInt))
        it "infers deref and addr of" $ do
            typeStringBlock "int x; int *y = &x; int* res = y;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int res = *y;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int** res = z;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int* res = *z;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int res = **z;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int a; *z = &a; **z = x;" `shouldBe` Nothing
        it "handles relaxed addr of" $ do
            typeStringBlock "int x; int *y = &x; int* res = &*y;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int** res = &*z;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; int* res = &**z;" `shouldBe` Nothing
        it "doesn't let you dereference an int" $ do
            typeStringBlock "int x; return *x;" `shouldBe` Just (BadDeref TInt)
            typeStringBlock "int x; int *y = &x; return **y;" `shouldBe` Just (BadDeref TInt)
--        it "doesn't allow bad addrOfs"
        it "infers arithmetic binops" $ do
            inferStringExpr "1 + 1" `shouldBe` Right TInt
            inferStringExpr "1 * 1" `shouldBe` Right TInt
            inferStringExpr "1 & 1" `shouldBe` Right TInt
            inferStringExpr "1 | 1" `shouldBe` Right TInt
            inferStringExpr "1 + 2 * 3" `shouldBe` Right TInt
        it "infers equality" $ do
            inferStringExpr "1 == 1" `shouldBe` Right TInt
            typeStringBlock "int x; int res = &x == &x;" `shouldBe` Nothing
        it "doesn't let you do equality on different types" $ do
            typeStringBlock "int x; return &x == x;" `shouldBe` Just (Mismatch (TRef TInt) TInt)
        it "handles assignments" $ do
            typeStringBlock "int x; int y; x = y;" `shouldBe` Nothing
            typeStringBlock "int *x; int *y; x = y;" `shouldBe` Nothing
            typeStringBlock "int *x; int y; x = y;" `shouldBe` Just (Mismatch (TRef TInt) TInt)
            typeStringBlock "int *x; int y; y = x;" `shouldBe` Just (Mismatch TInt (TRef TInt))
        it "handles dereference assignment" $ do
            typeStringBlock "int x; int *y = &x; *y = 2;" `shouldBe` Nothing
            typeStringBlock "int x; int *y = &x; int **z = &y; *z = &x; **z = 2;" `shouldBe` Nothing
            typeStringBlock "int x; *x = 2;" `shouldBe` Just (BadDeref TInt)
            typeStringBlock "int x; int *y = &x; **y = 2;" `shouldBe` Just (BadDeref TInt)
            typeStringBlock "int x; int *y = &x; *y = y;" `shouldBe` Just (Mismatch TInt (TRef TInt))
            typeStringBlock "int x; int *y = &x; int **z = &y; *z = x;" `shouldBe` Just (Mismatch (TRef TInt) TInt)
        it "handles arithmetic with dereferences" $ do
            typeStringBlock "int x; int *y; int **z; int res = 1 + (*y + x + **z) + *&x;" `shouldBe` Nothing
        it "handles arrays" $ do
            typeStringBlock "int[] xs; int[] res = xs;" `shouldBe` Nothing
            typeStringBlock "int[] xs; int res = xs[0];" `shouldBe` Nothing
            typeStringBlock "int[] xs = {1,2,3}; int res = xs[0];" `shouldBe` Nothing
            typeStringBlock "int x; int *y; int*[] xs = {y,y,&x}; int* res = xs[0];" `shouldBe` Nothing
            typeStringBlock "int x; int *y; int*[] xs = {y,y,&x}; int res = *xs[0];" `shouldBe` Nothing
        it "checks empty array literals" $ do
            typeStringBlock "int[] xs = {};" `shouldBe` Nothing
        it "doesn't let then and else have different non-void types" $ do
            typeStringProg "int main(){if(1) { return 1; } else { int *y; return y; }}" `shouldBe` Just (Mismatch TInt (TRef TInt))
        it "ensures if/loop condition is an int" $ do
            typeStringBlock "int *y; if(y) {}" `shouldBe` Just (Mismatch TInt (TRef TInt))
            typeStringBlock "int *y; while(y) {}" `shouldBe` Just (Mismatch TInt (TRef TInt))
            typeStringBlock "int *y; for(int x;y; x = 1;) {}" `shouldBe` Just (Mismatch TInt (TRef TInt))
        it "allows valid ifs" $ do
            -- TODO remove extra return after you fix statement checking
            typeStringProg "int main(){if(1) { return 1; } else { return 2; }}" `shouldBe` Nothing
        it "detects errors in if branches" $ do
            typeStringBlock "if(1) { return *1; }" `shouldBe` Just (BadDeref TInt)
            typeStringBlock "if(1) {} else { return *1; }" `shouldBe` Just (BadDeref TInt)
        it "detects wrong return type" $ do
            typeStringProg "int f() { int *x; return x; }" `shouldBe` Just (Mismatch TInt (TRef TInt))
            typeStringProg "int f(int x) { return &x; }" `shouldBe` Just (Mismatch TInt (TRef TInt))
            typeStringProg "void f() { return 1; }" `shouldBe` Just (Mismatch TVoid TInt)
        it "handles function calls" $ do
            typeStringProg "int f() { return 1; } int main() { int x = f(); return x; }" `shouldBe` Nothing
            typeStringProg "int f(int x, int *y) { return x + *y; } int main() { int a; int x = f(a,&a); return x; }" `shouldBe` Nothing
        it "handles argname same as funname" $ do
            typeStringProg "int f(int f) { return f; }" `shouldBe` Nothing
            typeStringProg "int f(int f) { return f(1); }" `shouldBe` Just (AppliedNonFunction TInt)
    describe "well formedness check" $ do
        it "detects unbound variables" $ do
            wfStringExpr "x" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "1 + (2 * x)" `shouldBe` [WF.UnboundVar "x"]
            wfStringExpr "1 + (2 * x)" `shouldBe` [WF.UnboundVar "x"]
            wfStringBlock "int x; return 2 + x + y;" `shouldBe` [WF.UnboundVar "y"]
            wfStringExpr "x + x + y" `shouldBe` [WF.UnboundVar "x", WF.UnboundVar "x", WF.UnboundVar "y"]
            wfStringBlock "int x = y; return 2 + x + y;" `shouldBe` [WF.UnboundVar "y", WF.UnboundVar "y"]
            wfStringBlock "int x = x; return 1;" `shouldBe` [WF.UnboundVar "x"]
            wfStringBlock "int[] xs = {1,2}; return xs;" `shouldBe` [] -- regression test
            wfStringBlock "x = 1; return 1;" `shouldBe` [WF.UnboundVar "x"]
        it "detects duplicate variables" $ do
            wfStringBlock "int x; int x; return 1;" `shouldBe` [WF.DupVar "x"]
            wfStringBlock "int x; int x = 1; return 1;" `shouldBe` [WF.DupVar "x"]
            wfStringBlock "int x = 1; int x = 1; return 1;" `shouldBe` [WF.DupVar "x"]
            wfStringBlock "int x = 1; int x; return 1;" `shouldBe` [WF.DupVar "x"]
            wfStringBlock "int x = 1; int y; int x; return 1;" `shouldBe` [WF.DupVar "x"]
            wfStringBlock "int x; int x; int y; int x; int y; return 1;" `shouldBe` (WF.DupVar <$> ["x", "x", "y"])
        it "knows assignments don't declare new vars" $ do
            wfStringBlock "int x = 1; x = 2; return 1;" `shouldBe` []
        it "knows where array literals belong where they don't" $ do
            wfStringBlock "int[] x = {1,2,3}; return 1;" `shouldBe` []
            wfStringBlock "int[] x; x = {1,2,3}; return 1;" `shouldBe` [WF.UnexpectedArrayLiteral (EArrayLiteral (EInt <$> [1,2,3]))]
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
            wfStringBlock "int[] xs; return xs[-1];" `shouldBe` [WF.NegativeIndex (EGetIndex (EVar "xs") (EUnop Neg (EInt 1)))]
            wfStringBlock "int[] xs; return xs[0];" `shouldBe` []
            wfStringBlock "int[] xs; return xs[0 + (-1)];" `shouldBe` []
        it "handles ifs" $ do
            wfStringBlock "if (x) { int x; return x + y; } else { return x; }" `shouldBe` (WF.UnboundVar <$> ["x","y","x"])
        it "has proper scoping for loop variables" $ do
            wfStringBlock "for(int x = 0; 1; x = x;) {} return x;" `shouldBe` [WF.UnboundVar "x"]
        it "detects errors in for parens" $ do
            wfStringBlock "for(int x = x; y; x = z;) {} return 1;"  `shouldBe` WF.UnboundVar <$> ["x","y","z"]
        it "detects errors in loop bodies" $ do
            wfStringBlock "for(int x =1; x; x = 1;) {int a = b; return z;} return 1;" `shouldBe` fmap WF.UnboundVar ["b","z"] ++ [WF.UnreachableStatement (Assign (LVar "x") (EInt 1))]
            wfStringBlock "while(1) {int a = b; return z;} return 1;" `shouldBe` WF.UnboundVar <$> ["b","z"]
        it "allows shadowing" $ do
            wfStringBlock "int x; if(1) { int x; } return 1;" `shouldBe` []
        it "allows arg name and funname to be the same" $ do
            wfStringProg "int f(int f) { return f; }" `shouldBe` []
        it "catches function dup vars" $ do
            wfStringProg "int f(int x, int x) { return x; }" `shouldBe` [WF.DupVar "x"]
            wfStringProg "int f(); int f() { return 1; }" `shouldBe` []
            wfStringProg "int f() { return 1; } int f() { return 1; }" `shouldBe` [WF.DupVar "f"]
        

{-
TODO test int f(int f) scope precedence
TODO test int f(int x, int x); 
TODO test dup fun def, dup fun decl, int f(); int f(){}  
-}