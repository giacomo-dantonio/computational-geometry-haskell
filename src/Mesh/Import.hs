module Mesh.Import ( fromObj ) where

    import Data.Vector (fromList)

    import Mesh.DataTypes
    import Mesh.IndexedFaceSet
    import Obj.Obj

    lineVertices :: ObjFileLine -> [Point3D Double]
    lineVertices (Vertex x y z) = [Point x y z]
    lineVertices (Vertex4 x y z w) = [Point x y z]
    lineVertices (UnnamedObject lines) = concatMap lineVertices lines
    lineVertices (Object _ lines) = concatMap lineVertices lines
    lineVertices (UnnamedGroup lines) = concatMap lineVertices lines
    lineVertices (Group _ lines) = concatMap lineVertices lines
    lineVertices _ = []

    lineFaces :: ObjFileLine -> [IndexedFace]
    lineFaces (Face vertexIndices) = [fromList [index vertex | vertex <- vertexIndices]]
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

    fromObj :: ObjFile -> IndexedFaceSet Double
    fromObj (Lines lines) =
        IndexedFaceSet {
            vertices = fromList vertexList,
            faces = fromList faceList
        }
        where
            vertexList = concatMap lineVertices lines
            faceList = concatMap lineFaces lines
