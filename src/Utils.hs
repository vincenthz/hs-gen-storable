module Utils where

import Data.Char

splitWhen     :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

toUpperFirst []     = []
toUpperFirst (x:xs) = toUpper x : xs

doCamelCase w = case splitWhen (== '_') w of
                    []     -> error "cannot camel case nothing"
                    [x]    -> x
                    (x:xs) -> concat (x : map toUpperFirst xs)

doCamelCaseType w = case splitWhen (== '_') w of
                    [] -> error "cannot camel case nothing"
                    x  -> concatMap toUpperFirst x
