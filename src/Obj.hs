module Obj where

import Data.List


data ObjVertex =
    Vertex Double Double Double
    | Vertex4 Double Double Double Double
    deriving Eq

instance Show ObjVertex where
    show (Vertex a b c) = "v " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (Vertex4 a b c d) = show (Vertex a b c) ++ " " ++ show d


data ObjTexture =
    Texture1 Double
    | Texture2 Double Double
    | Texture3 Double Double Double
    deriving Eq

instance Show ObjTexture where
    show (Texture1 a) = "vt " ++ show a
    show (Texture2 a b) = show (Texture1 a) ++ " " ++ show b
    show (Texture3 a b c) = show (Texture2 a b) ++ " " ++ show c


data ObjNormal = 
    Normal Double Double Double
    deriving Eq

instance Show ObjNormal where
    show (Normal a b c) = "vn " ++ show a ++ " " ++ show b ++ " " ++ show c


data ObjParameter = 
    Parameter1 Double
    | Parameter2 Double Double
    | Parameter3 Double Double Double
    deriving Eq

instance Show ObjParameter where
    show (Parameter1 a) = "vp " ++ show a
    show (Parameter2 a b) = show (Parameter1 a) ++ " " ++ show b
    show (Parameter3 a b c) = show (Parameter2 a b) ++ " " ++ show c

data ObjVertexIndex = 
    VertexIndex Int
    | VertexTexture Int Int
    | VertexNormal Int Int
    | VertexTextureNormal Int Int Int
    deriving Eq

instance Show ObjVertexIndex where
    show (VertexIndex a) = show a
    show (VertexTexture a b) = show a ++ "/" ++ show b
    show (VertexNormal a b) = show a ++ "//" ++ show b
    show (VertexTextureNormal a b c) = show a ++ "/" ++ show b ++ "/" ++ show c

data ObjFace = Face [ObjVertexIndex]
    deriving Eq

instance Show ObjFace where
    show (Face vertices) = "f " ++ (intercalate " " $ map show vertices)

data ObjLine = Line [Int]
    deriving Eq

instance Show ObjLine where
    show (Line vertices) = "l " ++ (intercalate " " $ map show vertices)

data ObjFile = File
    { vertices   :: [ObjVertex]
    , textures   :: [ObjTexture]
    , normals    :: [ObjNormal]
    , parameters :: [ObjParameter]
    , faces      :: [ObjFace]
    , lines      :: [ObjLine]
    }
