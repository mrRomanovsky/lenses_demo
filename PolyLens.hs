{-# LANGUAGE RecordWildCards, Rank2Types #-}

module PolyLens where

import PolyParser
import Control.Lens

degree :: Lens' Poly Int
degree f p@Poly{_coef = q, _deg = d} = f d <&> shift_coef
  where shift_coef fd
          | fd == d = p
          | fd < d = p {_deg = fd}
          | otherwise = Poly {_coef = replicate (fd - d) 0 ++ q, _deg = fd}


formal_degree :: Lens' Poly Int
formal_degree f p@Poly{_deg = d, _coef = q} = 
    let ddif = n_zero_idx q
        n_zero_idx [] = error "Polynomial should have at least 1 coefficient!"
        n_zero_idx [x] = 0
        n_zero_idx (x:xs)
          | x == 0 = 1 + (n_zero_idx xs)
          | otherwise = 0
        in case ddif of
          0 -> degree f p
          _ -> f (d - ddif) <&> \dg -> if dg > d - ddif
            then error "You can't increase formal _degree!"
            else p {_deg = dg + ddif}


coef_degree :: Int -> Traversal' Poly Float
coef_degree d f p@Poly{_deg = pd, ..} = tr f p
  where tr = coef . ix (pd - d)
