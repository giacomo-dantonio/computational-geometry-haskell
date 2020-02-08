module Main where

import Text.ParserCombinators.ReadP
import Obj.Parse

-- FIXME: print is escaping the newlines, which I don't want to.
main :: IO ()
main = do
    text <- readFile "./files/teapot.obj"
    let parsedObj = readP_to_S parseFile text
    print parsedObj
