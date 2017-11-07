module ComplexParser where
import Control.Applicative hiding (many, optional)
import Control.Monad
import Parser
import SimpleParsers
import ParseNumbers
import Data.Char

{- Напишите парсер для вещественных чисел. -}

isInt :: String -> Bool
isInt s
  | (head s == '-') = all isDigit $ tail s
  | otherwise       = all isDigit s

intFloat = fromIntegral :: Int -> Float

digitsCount :: Int -> Int
digitsCount = length . show . abs

float :: Parser Float
float = do
    r <- intFloat <$> integer
    parseF r <|> (return r)
  where
    parseF r = do
      char '.'
      f <- natural
      let fLen = digitsCount f
      return $ r + intFloat f / fromIntegral  10 ^ fLen
{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}

sepComma :: Parser (Float, Float)
sepComma = do
    r <- float
    string ", "
    i <- float
    return (r, i)

complex :: Parser (Float, Float)
complex = bracket "(" ")" sepComma

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}


--}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token $ complex <|> float0) (symbol ";")
  where
    float0 = do
        f <- float
        return (f, 0 :: Float)

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token $ complex <|> float0) (symbol ",")
  where
    float0 = do
        f <- float
        return (f, 0 :: Float)