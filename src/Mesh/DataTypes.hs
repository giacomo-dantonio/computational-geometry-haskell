module Mesh.DataTypes (
    Point3D (..)
,   IndexedFace
)where

    import Data.Array.IArray

    data Point3D a = Point a a a
    type IndexedFace = Array Int Int