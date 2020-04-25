module Main where

import Text.ParserCombinators.ReadP
import Obj.Parse
import Obj.Obj
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = do
    text <- readFile "./files/airboat.obj" 
    let parsedObj = readP_to_S parseFile text
    let maybeFile = listToMaybe [ x | (x, _) <- parsedObj ]
    let text = fromMaybe "Unable to parse file: wrong format" $ show <$> maybeFile
    putStrLn text
