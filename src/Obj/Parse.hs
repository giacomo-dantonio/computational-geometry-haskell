module Obj.Parse where

import Control.Applicative ((<|>), liftA)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Obj.Obj

-- Parse combinators for an OBJ file.

-- TODO:
-- - add support groups and sections
-- - add support for materials
-- - add support for comments (just ignore them)

-- Parse numbers.

parseSign :: (Num a) => ReadP a
parseSign = do
    sign <- option '+' (char '-')
    return $ if sign == '+' then 1 else -1

parseInteger :: ReadP Int
parseInteger = do
    sign <- parseSign
    s <- many1 $ satisfy isDigit
    return $ sign * read s

parseDouble :: ReadP Double
parseDouble = (do
    sign <- parseSign
    int <- many1 $ satisfy isDigit
    char '.'
    decimal <- many1 $ satisfy isDigit
    let s = int ++ "." ++ decimal
    return $ sign * read s
    )

parseNumber = fmap fromIntegral parseInteger <|> parseDouble


-- Parse elements of an OBJ file.

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


-- Parse any line of an OBJ file.

parseLine :: ReadP ObjFileLine
parseLine =
    liftA V vertex
    <|> liftA VT texture
    <|> liftA VN normal
    <|> liftA VP parameter
    <|> liftA F face
    <|> liftA L objPolyline

-- An empty OBJ file.

emptyFile :: ObjFile
emptyFile = File {
    vertices = [],
    textures = [],
    normals = [],
    parameters = [],
    faces = [],
    polylines = []
}

-- Fold a list of lines to an obj file.

fileFromLines :: [ObjFileLine] -> ObjFile
fileFromLines = foldl addLine emptyFile
    where
        addLine file (V vertex) = file { vertices = vertices file ++ [vertex] }
        addLine file (VT texture) = file { textures = textures file ++ [texture] }
        addLine file (VN normal) = file { normals = normals file ++ [normal] }
        addLine file (VP parameter) = file { parameters = parameters file ++ [parameter] }
        addLine file (F face) = file { faces = faces file ++ [face] }
        addLine file (L line) = file { polylines = polylines file ++ [line] }

-- Parse an OBJ file.

parseFile :: ReadP ObjFile
parseFile = fileFromLines <$> many (do
    line <- parseLine
    char '\n'
    return line)
    