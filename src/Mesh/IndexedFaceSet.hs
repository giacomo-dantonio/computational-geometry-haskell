module Mesh.IndexedFaceSet (
    IndexedFaceSet (..)
,   IndexedFace
) where

    import Mesh.DataTypes
    import Data.Vector (Vector, length)
    import Prelude hiding (length)
    
    type IndexedFace = Vector Int

    data IndexedFaceSet a = IndexedFaceSet
        {
            vertices :: Vector (Point3D a)
        ,   faces    :: Vector IndexedFace
        }


    nVertices :: IndexedFaceSet a -> Int
    nVertices mesh = Data.Vector.length $ vertices mesh

    nFaces :: IndexedFaceSet a -> Int
    nFaces mesh = Data.Vector.length $ faces mesh

    instance Show (IndexedFaceSet a) where
        show mesh =
            "Mesh (#vertices "
            ++ show (nVertices mesh)
            ++ ", #faces "
            ++ show (nFaces mesh)
            ++ ")"
