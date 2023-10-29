module Expression where
import           Control.Monad.State (MonadIO (liftIO), StateT)
import           Data.Fixed          (mod')
import           Data.Map            (Map)
import           Numeric             (showFFloat)


data Expression = Constant Double
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Modulo Expression Expression
                | Negation Expression
                  deriving (Show)

data Statement = PrintStatement Expression deriving (Show)

type Calculator a = StateT (Map String Expression) IO a

evalExpression :: Expression -> Calculator Double

evalExpression (Constant n) = return n

evalExpression (Addition a' b') = do
  a <- evalExpression a'
  b <- evalExpression b'
  return (a + b)

evalExpression (Subtraction a' b') = do
  a <- evalExpression a'
  b <- evalExpression b'
  return (a - b)

evalExpression (Multiplication a' b') = do
  a <- evalExpression a'
  b <- evalExpression b'
  return (a * b)

evalExpression (Division a' b') = do
  a <- evalExpression a'
  b <- evalExpression b'
  return (a / b)

evalExpression (Modulo a' b') = do
  a <- evalExpression a'
  b <- evalExpression b'
  return (mod' a b)

evalExpression (Negation a') = do
  a <- evalExpression a'
  return (negate a)

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

printStatement :: Statement -> Calculator ()
printStatement (PrintStatement s) = do
  result <- evalExpression s
  liftIO . putStrLn . showFullPrecision $ result
