module Main where

import Data.Array (bounds)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.ParserCombinators.ReadP (readP_to_S)

import Obj.Parse
import Mesh.Import
import Mesh.Mesh

main :: IO ()
main = do
    text <- readFile "./files/airboat.obj" 
    let parsedObj = readP_to_S parseFile text
    let maybeFile = listToMaybe [ x | (x, _) <- parsedObj ]
    let maybeMesh = fromObj <$> maybeFile
    let nVertices = fromMaybe 0 $ snd . bounds <$> vertices <$> maybeMesh
    let nFaces = fromMaybe 0 $ snd . bounds <$> faces <$> maybeMesh
    let text = "Mesh parsed. " ++ show nVertices ++ " vertices, " ++ show nFaces ++ " faces."
    putStrLn text
