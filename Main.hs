{-# LANGUAGE TemplateHaskell #-}

import Control.Lens


data SmallDigitsCounter = SmallDigitsCounter {_one :: Int, _two :: Int, _three :: Int, _four :: Int} deriving (Show)

data BigDigitsCounter = BigDigitsCounter {_five :: Int, _six :: Int, _seven :: Int, _nine :: Int} deriving (Show)

data DigitsCounter = DigitsCounter {_smallDigits :: SmallDigitsCounter, _bigDigits :: BigDigitsCounter} deriving (Show)

data VowelsCounter = VowelsCounter {_a :: Int, _e :: Int, _i :: Int, _o :: Int} deriving (Show)

data ConsonantsCounter = ConsonantsCounter {_b :: Int, _c :: Int, _d :: Int, _f :: Int} deriving (Show)

data SymbsCounter = SymbsCounter {_vowels :: VowelsCounter, _consonants :: ConsonantsCounter} deriving (Show)

data Counter = Counter {_digits :: DigitsCounter, _symbs :: SymbsCounter} deriving (Show)

$(makeLenses ''DigitsCounter)
$(makeLenses ''Counter)
$(makeLenses ''SmallDigitsCounter)
$(makeLenses ''BigDigitsCounter)
$(makeLenses ''VowelsCounter)
$(makeLenses ''ConsonantsCounter)
$(makeLenses ''SymbsCounter)

initCounter :: Counter
initCounter = let
                  sD = SmallDigitsCounter 0 0 0 0
                  bD = BigDigitsCounter 0 0 0 0
                  vC = VowelsCounter 0 0 0 0
                  cS = ConsonantsCounter 0 0 0 0
                  dC = DigitsCounter sD bD
                  sC = SymbsCounter vC cS
              in Counter dC sC


increaseOnes :: Counter -> Counter
increaseOnes = symbs . vowels . a +~ 1

main = do
  putStrLn $ "Initial counter :" ++ show initCounter
  putStrLn "-----------------------------------------"
  putStrLn $ "initCounter ^. digits ^. smallDigits ^. one = " ++ (show $ initCounter ^. digits ^. smallDigits)
  putStrLn "-----------------------------------------"
  putStrLn $ "after incrementing initCounter ^. symbs ^. vowels ^. a : "  ++ (show $ increaseOnes initCounter)
  

