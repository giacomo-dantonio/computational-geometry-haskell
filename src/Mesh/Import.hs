module Mesh.Import ( fromObj ) where

    import Data.Array
    import Mesh.DataTypes
    import Mesh.Mesh
    import Obj.Obj

    lineVertices :: ObjFileLine -> [Point3D Double]
    lineVertices (Vertex x y z) = [Point x y z]
    lineVertices (Vertex4 x y z w) = [Point x y z]
    lineVertices (UnnamedObject lines) = concatMap lineVertices lines
    lineVertices (Object _ lines) = concatMap lineVertices lines
    lineVertices (UnnamedGroup lines) = concatMap lineVertices lines
    lineVertices (Group _ lines) = concatMap lineVertices lines
    lineVertices _ = []

    lineFaces :: ObjFileLine -> [MeshFace]
    lineFaces (Face vertexIndices) = [listArray (0, length vertexIndices - 1) [index vertex | vertex <- vertexIndices]]
        where
            index :: ObjVertexIndex -> Int
            index (VertexIndex i) = i
            index (VertexTexture i _) = i
            index (VertexNormal i _) = i
            index (VertexTextureNormal i _ _) = i
    lineFaces (UnnamedObject lines) = concatMap lineFaces lines
    lineFaces (Object _ lines) = concatMap lineFaces lines
    lineFaces (UnnamedGroup lines) = concatMap lineFaces lines
    lineFaces (Group _ lines) = concatMap lineFaces lines
    lineFaces _ = []

    fromObj :: ObjFile -> Mesh Double
    fromObj (Lines lines) =
        Mesh {
            vertices = listArray (0, length vertexList) vertexList,
            faces = listArray (0, length faceList) faceList
        }
        where
            vertexList = concatMap lineVertices lines
            faceList = concatMap lineFaces lines
