module Mesh.IndexedFaceSet (
    IndexedFaceSet (..)
) where

    import Mesh.DataTypes
    import Data.Array.IArray
    
    data IndexedFaceSet a = IndexedFaceSet
        {
            vertices :: Array Int (Point3D a)
        ,   faces    :: Array Int IndexedFace
        }


    nVertices :: IndexedFaceSet a -> Int
    nVertices mesh = snd $ bounds $ vertices mesh

    nFaces :: IndexedFaceSet a -> Int
    nFaces mesh = snd $ bounds $ faces mesh

    instance Show (IndexedFaceSet a) where
        show mesh =
            "Mesh (#vertices "
            ++ show (nVertices mesh)
            ++ ", #faces "
            ++ show (nFaces mesh)
            ++ ")"
