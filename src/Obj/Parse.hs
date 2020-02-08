module Obj.Parse where

import Control.Applicative
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Obj.Obj


parseInteger :: ReadP Int
parseInteger = do
    s <- many1 $ satisfy isDigit
    return $ read s

parseDouble :: ReadP Double
parseDouble = (do
    int <- many1 $ satisfy isDigit
    char '.'
    decimal <- many1 $ satisfy isDigit
    let s = int ++ "." ++ decimal
    return $ read s
    )

parseNumber = fmap fromIntegral parseInteger <|> parseDouble

vertex :: ReadP ObjVertex
vertex = do
    string "v "
    numbers <- sepBy parseNumber skipSpaces
    case numbers of
        [a, b, c]    -> return $ Vertex a b c
        [a, b, c, d] -> return $ Vertex4 a b c d
        _            -> fail ""


texture :: ReadP ObjTexture
texture = do
    string "vt "
    numbers <- sepBy parseNumber skipSpaces
    case numbers of
        [a]       -> return $ Texture1 a
        [a, b]    -> return $ Texture2 a b
        [a, b, c] -> return $ Texture3 a b c
        _         -> fail ""

normal :: ReadP ObjNormal
normal = do
    string "vn "
    numbers <- sepBy parseNumber skipSpaces
    case numbers of
        [a, b, c] -> return $ Normal a b c
        _         -> fail ""

parameter :: ReadP ObjParameter
parameter = do
    string "vp "
    numbers <- sepBy parseNumber skipSpaces
    case numbers of
        [a]       -> return $ Parameter1 a
        [a, b]    -> return $ Parameter2 a b
        [a, b, c] -> return $ Parameter3 a b c
        _         -> fail ""

vertexIndex :: ReadP ObjVertexIndex
vertexIndex = do
        a <- parseInteger
        return $ VertexIndex a
    <|> do
        a <- parseInteger
        char '/'
        b <- parseInteger
        return $ VertexTexture a b
    <|> do
        a <- parseInteger
        string "//"
        b <- parseInteger
        return $ VertexNormal a b
    <|> do
        a <- parseInteger
        char '/'
        b <- parseInteger
        char '/'
        c <- parseInteger
        return $ VertexTextureNormal a b c

face :: ReadP ObjFace
face = do
    string "f "
    vertices <- sepBy vertexIndex skipSpaces
    return $ Face vertices

objPolyline :: ReadP ObjPolyLine
objPolyline = do
    string "l "
    elements <- sepBy parseInteger skipSpaces
    return $ Line elements

parseLine :: ReadP ObjFileLine
parseLine =
    liftA V vertex
    <|> liftA VT texture
    <|> liftA VN normal
    <|> liftA VP parameter
    <|> liftA F face
    <|> liftA L objPolyline

emptyFile :: ObjFile
emptyFile = File {
    vertices = [],
    textures = [],
    normals = [],
    parameters = [],
    faces = [],
    polylines = []
}

--fileFromLines :: [ObjFileLine] -> ObjFile
--fileFromLines = foldl add