import Control.Lens
import PolyParser
import PolyLens

p = Poly {_coef = [0, 3, 1, 2], _deg = 3}

t1 = p & degree %~ (+1)
t2 = p & degree %~ (subtract 1)
t3 = p & formal_degree %~ (subtract 1)
t4 = p & (coef_degree 1) %~ (*8)
t5 = p & (coef_degree 25) %~ (+27)
t6 = p & (coef_degree 25) (\x -> Just $ x + 1)
t7 = p & (coef_degree 25) (\_ -> Nothing)
t8 = p & (coef_degree 2) (\_ -> Nothing)

tests = p ^. degree == 3
     && p ^. formal_degree == 2
     && p ^? (coef_degree 1) == (Just 1)
     && p ^? (coef_degree 42) == Nothing
     && t1 == Poly {_coef = [0, 0, 3, 1, 2], _deg = 4}
     && t2 == Poly {_coef = [0, 3, 1, 2], _deg = 2}
     && t3 ^. formal_degree == 1  
     && t4 == Poly {_coef = [0, 3, 8, 2], _deg = 3}
     && t5 == p
     && t6 == (Just p)
     && t7 == (Just p)
     && t8 == Nothing

main = do
    putStrLn $ "tests completed succesfully: " ++ (show tests)