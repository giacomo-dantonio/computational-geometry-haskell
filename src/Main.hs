module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Text.ParserCombinators.ReadP (readP_to_S)

import Obj.Parse
import Mesh.Import
import Mesh.IndexedFaceSet

main :: IO ()
main = do
    text <- readFile "./files/airboat.obj" 
    let parsedObj = readP_to_S parseFile text
    let maybeFile = listToMaybe [ x | (x, _) <- parsedObj ]
    let maybeMesh = fromObj <$> maybeFile
    let text = fromMaybe "Error: unable to parse Obj file." $ show <$> maybeMesh
    putStrLn text
