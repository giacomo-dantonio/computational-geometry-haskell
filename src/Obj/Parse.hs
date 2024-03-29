module Obj.Parse ( parseFile ) where


import Control.Applicative ((<|>), liftA)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Obj.Obj

-- Parser combinators for an OBJ file.

-- TODO:
-- - add support for smooths
-- - add support for materials

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

vertex :: ReadP ObjFileLine
vertex = do
    string "v "
    numbers <- sepBy parseNumber $ char ' '
    case numbers of
        [a, b, c]    -> return $ Vertex a b c
        [a, b, c, d] -> return $ Vertex4 a b c d
        _            -> fail ""


texture :: ReadP ObjFileLine
texture = do
    string "vt "
    numbers <- sepBy parseNumber $ char ' '
    case numbers of
        [a]       -> return $ Texture1 a
        [a, b]    -> return $ Texture2 a b
        [a, b, c] -> return $ Texture3 a b c
        _         -> fail ""

normal :: ReadP ObjFileLine
normal = do
    string "vn "
    numbers <- sepBy parseNumber $ char ' '
    case numbers of
        [a, b, c] -> return $ Normal a b c
        _         -> fail ""

parameter :: ReadP ObjFileLine
parameter = do
    string "vp "
    numbers <- sepBy parseNumber $ char ' '
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

face :: ReadP ObjFileLine
face = do
    string "f "
    vertices <- sepBy vertexIndex $ char ' '
    return $ Face vertices

objPolyline :: ReadP ObjFileLine
objPolyline = do
    string "l "
    elements <- sepBy parseInteger $ char ' '
    return $ Line elements

-- Parse lines of an OBJ file

parseLine :: ReadP ObjFileLine
parseLine =
    vertex
    <|> texture
    <|> normal
    <|> parameter
    <|> face
    <|> objPolyline
    <|> do
        skipMany $ char ' '
        return Empty
    <|> do
        char '#'
        skipMany (satisfy $ (/=) '\n')
        return Empty
    -- skip not yet supported features
    <|> do
        string "mtllib" <|> string "usemtl" <|> string "s" <|> string "o"
        skipMany (satisfy $ (/=) '\n')
        return Empty

parseLines :: ReadP [ObjFileLine]
parseLines = many (do
    line <- parseLine
    char '\n'
    return line)

-- Parse groups
-- here we assume that nested groups are not allowed

parseGroup :: ReadP ObjFileLine
parseGroup = (
    do
        string "g "
        name <- munch ((/=) '\n')
        char '\n'
        lines <- parseLines
        return $ Group name lines
    ) <|> (do
        char 'g'
        char '\n'
        lines <- parseLines
        return $ UnnamedGroup lines
    )

parseGroups :: ReadP [ObjFileLine]
parseGroups = many parseGroup

-- Parse objects
-- here we assume that nested objects are not allowed

parseObject :: ReadP ObjFileLine
parseObject = (
    do
        string "o "
        name <- munch ((/=) '\n')
        char '\n'
        content <- parseContent
        return $ Object name content
    ) <|> (do
        char 'o'
        char '\n'
        content <- parseContent
        return $ UnnamedObject content
    )
    where
        parseContent = do
            lines <- parseLines
            groups <- parseGroups
            return $ lines ++ groups       

parseObjects :: ReadP [ObjFileLine]
parseObjects = many parseObject

-- Parse an OBJ file.

parseFileLines :: ReadP [ObjFileLine]
parseFileLines = do
    lines <- parseLines
    groups <- parseGroups
    objects <- parseObjects
    return $ lines ++ groups ++ objects

parseFile :: ReadP ObjFile
parseFile = do
    file <- parseFileLines
    eof
    return $ Lines file
