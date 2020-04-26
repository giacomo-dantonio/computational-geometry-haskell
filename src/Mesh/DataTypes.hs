module Mesh.DataTypes (
    Point3D (..)
,   MeshFace
)where

    import Data.Array.IArray

    data Point3D a = Point a a a
    type MeshFace = Array Int Int