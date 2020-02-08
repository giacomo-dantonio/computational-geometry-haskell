module Obj.Obj where

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

    
data ObjPolyLine = Line [Int]
    deriving Eq

instance Show ObjPolyLine where
    show (Line vertices) = "l " ++ (intercalate " " $ map show vertices)

    
data ObjFile = File
    { vertices   :: [ObjVertex]
    , textures   :: [ObjTexture]
    , normals    :: [ObjNormal]
    , parameters :: [ObjParameter]
    , faces      :: [ObjFace]
    , polylines  :: [ObjPolyLine]
    }

instance Show ObjFile where
    show file = unlines $ contact [
        fmap show (vertices file),
        fmap show (textures file),
        fmap show (normals file),
        fmap show (parameters file),
        fmap show (faces file),
        fmap show (polylines file)]   

data ObjFileLine =
    V ObjVertex
    | VT ObjTexture
    | VN ObjNormal
    | VP ObjParameter
    | F ObjFace
    | L ObjPolyLine
    deriving Eq

instance Show ObjFileLine where
    show (V v) = show v
    show (VT t) = show t
    show (VN n) = show n
    show (VP p) = show p
    show (F f) = show f
    show (L l) = show l

