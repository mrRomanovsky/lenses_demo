{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PolyParser where

import Control.Applicative hiding (many, optional)
import Control.Monad
import Control.Lens
import Parser
import SimpleParsers
import ParseNumbers
import ComplexParser
import System.Environment
import System.Directory
{-
   Определите тип для многочлена с вещественными коэффициентами.
-}

data Poly = Poly {_coef :: [Float], _deg :: Int} deriving (Show, Eq)

$(makeLenses ''Poly)

intZ = 0 :: Int
flZero = 0.0 :: Float

{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

lstToPoly :: [(Float, Int)] -> Poly
lstToPoly [] = error "Polynomial should have at least 1 _coefficient!"
lstToPoly [(x, 0)] = Poly {_coef = [x], _deg = 0}
lstToPoly [(x, n)] = Poly {_coef = x : replicate n flZero, _deg = n}
lstToPoly ((x, n) : (y, m) : xs)
  | n == m + 1 = let qy = _coef $ lstToPoly $ (y, m) : xs
                     in Poly{_coef = x : qy, _deg = n}
  | otherwise  = let qy = _coef $ lstToPoly $ (y, m) : xs
                     qx = x : replicate (n - m) flZero
                     in Poly{_coef = qx ++ qy, _deg = n}

poly :: Parser Poly
poly = do
   qfs <- _degs
   return $ lstToPoly qfs


_degs :: Parser [(Float, Int)]
_degs = do
  qd <- q_deg --отдельно обрабатываем первый "самый левый" элемент - 
             -- - перед ним может не быть знаков "+" или "-"
  qds <- many _coef_deg
  return $ qd : qds

_coef_deg :: Parser (Float, Int)
_coef_deg = do
  space
  x <- getc
  space
  (q, d) <- q_deg
  let qf = if x == '-' then (-1) * q else q
  return (qf, d)

q_deg :: Parser (Float, Int)
q_deg = do
   q <- float <|> (return 1.0)
   d <- (string "x^" >> integer) <|> (return intZ)
   return (q, d)
{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}



mult :: Float -> Poly -> Poly
n `mult` Poly{_coef = q, _deg = d} = Poly{_coef = map (* n) q, _deg = d}


getPoly :: [Float] -> Poly
getPoly [] = error "Polynomial should have at least 1 _coefficient!"
getPoly (x:[]) = Poly{_coef = [x], _deg = 0}
getPoly l@(x:y:xs)
  | x /= 0 = Poly{_coef = l, _deg = length l - 1}
  | otherwise = getPoly $ tail l


minus :: Poly -> Poly -> Poly
Poly{_coef = q1, _deg = d1} `minus` Poly{_coef = q2, _deg = d2} =
 let dif = zipWith (-) q1 q2
     in getPoly dif
{--minus :: Poly -> Poly -> Poly
Poly{_coef = q1, _deg = d1} `minus` Poly{_coef = q2, _deg = d2}
  | d1 == d2 = let dif = zipWith (-) q1 q2
                         in getPoly dif
  | d1 > d2 = let dDif = d1 - d2
                  nQ2 = replicate dDif 0 : q2
                  in getPoly (zipWith (-) q1 q2) d1
  | otherwise = 
--}
multXn :: Poly -> (Float, Int) -> Poly
multXn Poly{_coef = q, _deg = d} (x, d2) =
   let newQ = map (*x) $ q ++ replicate d2 flZero
   in Poly{_coef = newQ, _deg = d + d2}

divmod :: Poly -> Poly -> (Poly, Poly)
divmod p1@Poly{_coef = q1, _deg = d1} p2@Poly{_coef = q2, _deg = d2}
  | d1 < d2 = (Poly{_coef = [flZero], _deg = 0}, p2)
  | d1 == d2 = let q = (head q1) / (head q2)
                   in (Poly{_coef = [q], _deg = 0}, p1 `minus` (q `mult` p2))
  | otherwise = let q = (head q1) / (head q2)
                    df = d1 - d2
                    p2inc = p2 `multXn` (q, df)
                    pm@Poly{_deg = d,..} = p1 `minus` p2inc
                    in if d < d2
                     then 
                       let pd = Poly{_coef = q : replicate df flZero, _deg = df}
                           in (pd, pm)
                     else
                       let (ddiv@Poly{_coef = qd, _deg=dd}, dmod) = divmod pm p2
                           in (Poly{_coef = q:qd, _deg = df + dd}, dmod)

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd Poly{_coef = [flZero], ..} p2 = p2
poly_gcd p1 Poly{_coef = [flZero], ..} = p1 
poly_gcd p1 p2 = poly_gcd p2 $ snd $ divmod p1 p2 

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Either String Poly] -> Either String Poly 
poly_gcd_list = foldl1 either_gcd
  where
   either_gcd (Left s) _ = Left s
   either_gcd _ (Left s) = Left s
   either_gcd (Right p1) (Right p2) = Right $ poly_gcd p1 p2

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file fp = do
   lns <- lines <$> readFile fp
   let polys = map eitherParse lns
       eitherParse s = case apply poly s of
         [] -> Left $ "parsing error! " ++ s
         xs -> Right $ fst $ head xs
   return $ poly_gcd_list polys

usage :: IO ()
usage = putStrLn "usage : 05-polynomials <filepath> - try to calculate gcd of polynomials from file"

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}