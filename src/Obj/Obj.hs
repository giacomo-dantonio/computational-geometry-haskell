module Obj.Obj where

-- Data types for the elements of an obj file
-- see https://en.wikipedia.org/wiki/Wavefront_.obj_file#File_format

-- TODO:
-- - add support groups and sections
-- - add support for materials

import Data.List

-- face connectivites

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


data ObjFileLine = 
    -- mesh vertices
    Vertex Double Double Double
    | Vertex4 Double Double Double Double
    -- uv coordinates for textures
    | Texture1 Double
    | Texture2 Double Double
    | Texture3 Double Double Double
    -- face normals
    | Normal Double Double Double
    -- parameter space
    | Parameter1 Double
    | Parameter2 Double Double
    | Parameter3 Double Double Double
    -- faces
    | Face [ObjVertexIndex]
    -- polylines
    | Line [Int]
    -- empty line or comment
    | Empty
    deriving Eq

instance Show ObjFileLine where
    show (Vertex a b c) = "v " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (Vertex4 a b c d) = show (Vertex a b c) ++ " " ++ show d
    show (Texture1 a) = "vt " ++ show a
    show (Texture2 a b) = show (Texture1 a) ++ " " ++ show b
    show (Texture3 a b c) = show (Texture2 a b) ++ " " ++ show c
    show (Normal a b c) = "vn " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (Parameter1 a) = "vp " ++ show a
    show (Parameter2 a b) = show (Parameter1 a) ++ " " ++ show b
    show (Parameter3 a b c) = show (Parameter2 a b) ++ " " ++ show c
    show (Face vertices) = "f " ++ (intercalate " " $ map show vertices)
    show (Line vertices) = "l " ++ (intercalate " " $ map show vertices)
    show Empty = ""

newtype ObjFile = Lines [ObjFileLine]

instance Show ObjFile where
    show (Lines filelines) = unlines [show line | line <- filelines]
