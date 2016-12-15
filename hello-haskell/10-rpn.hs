{-
import Control.Monad

main = iRPN EmptyRPN

iRPN :: RPN -> IO ()
iRPN RPN = do
    line <- getLine
    when (not $ null line) $ do
    
-}      

type Value = Double
data Operator = Add | Subtract | Multiply | Divide | Raise deriving (Show, Read)
data RPN = EmptyRPN | Stack (Maybe Value) [Value] deriving (Show)

pushVal :: Value -> RPN -> RPN
pushVal v EmptyRPN = Stack Nothing [v]
pushVal v (Stack a vs) = Stack a (v:vs)

pushOp :: Operator -> RPN -> RPN
pushOp o EmptyRPN = error "Cannot push operator onto an empty stack"
pushOp o (Stack Nothing (v1:v2:vs)) = Stack (Just (applyOp o v1 v2)) vs
pushOp o (Stack (Just v1) (v2:vs)) = Stack (Just (applyOp o v1 v2)) vs
pushOp _ _ = error "Not enough values on the stack to perform operation"

applyOp :: Operator -> Value -> Value -> Value
applyOp Add a b = (a + b) :: Value
applyOp Subtract a b = (a - b) :: Value
applyOp Multiply a b = (a * b) :: Value
applyOp Divide a b = (a / b) :: Value
applyOp Raise a b = a ** b :: Value

getAns :: RPN -> Maybe Value
getAns EmptyRPN = Nothing
getAns (Stack x _) = x
