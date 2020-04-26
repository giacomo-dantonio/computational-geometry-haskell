module Mesh.Mesh (
    Mesh (..)
) where

    import Mesh.DataTypes
    import Data.Array.IArray
    
    data Mesh a = Mesh
        {
            vertices :: Array Int (Point3D a)
        ,   faces    :: Array Int MeshFace
        }
