module Mesh.HalfEdge where

    import Data.List (union)
    import qualified Data.Vector as Vector
    import Data.Vector (Vector)
    import qualified Data.HashMap.Strict as HashMap
    import Data.HashMap.Strict (HashMap, (!))

    import Mesh.DataTypes (Point3D)
    import qualified Mesh.IndexedFaceSet as IFS
    import Mesh.IndexedFaceSet (IndexedFaceSet (..), IndexedFace)

    data HalfEdge = HalfEdge
        {
            previous :: Int
        ,   next :: Int
        ,   vertex :: Int
        ,   face :: Int
        ,   opposite :: Int
        }

    data HEFace = HEFace
        {
            faceHalfEdge :: Int
        }
    
    data HEVertex a = HEVertex
        {
            point :: Point3D a
        ,   vertexHalfEdge :: Int
        }

    data HalfEdgeMesh a = HalfEdgeMesh
        {
            vertices :: Vector (HEVertex a)
        ,   faces :: Vector HEFace
        ,   halfEdges :: Vector HalfEdge
        }
    
    shift :: [a] -> [a]
    shift [] = []
    shift (head : tail) = tail ++ [head]

    -- The edges of the face as pairs (i, j) of vertex indices
    -- with the property that i <= j
    faceEdges :: IndexedFace -> [(Int, Int)]
    faceEdges indices = [ (min i j, max i j) | (i, j) <- pairs ]
        where
            listIndices = Vector.toList indices
            pairs = zip listIndices (shift listIndices)

    -- All the mesh edges as pairs (i, j) of vertex indices
    -- with the property that i <= j
    edgePairs :: [IndexedFace] -> [(Int, Int)]
    edgePairs faces = foldr union [] $ faceEdges <$> faces

    -- All half edges as triples (faceIndex, i, j)
    -- where i, j are vertex indices
    -- with the property that i <= j
    halfEdgeTriples :: [IndexedFace] -> [(Int, Int, Int)]
    halfEdgeTriples faces = concat $ zipWith makeTriples [0 ..] (faceEdges <$> faces)
        where
            makeTriple :: Int -> (Int, Int) -> (Int, Int, Int)
            makeTriple faceIndex (i, j) = (faceIndex, i, j)

            makeTriples :: Int -> [(Int, Int)] -> [(Int, Int, Int)]
            makeTriples faceIndex pairs = makeTriple faceIndex <$> pairs

    -- A map between triples (faceIndex, i, j) and half edge indices
    halfEdgeIndices :: [IndexedFace] -> HashMap (Int, Int, Int) Int
    halfEdgeIndices faces = HashMap.fromList $ zip (halfEdgeTriples faces) [0 ..]

    -- A map between half edge indices and triples (faceIndex, i, j)
    halfEdgeMap :: [IndexedFace] -> HashMap Int (Int, Int, Int)
    halfEdgeMap faces = HashMap.fromList $ zip [0 ..] (halfEdgeTriples faces)


    -- A map between an edge (i, j) with the property i <= j
    -- and the faces on it
    edgeFacesMap :: [IndexedFace] -> HashMap (Int, Int) [Int]
    edgeFacesMap faces = 
        HashMap.fromList $ [ (pair, faceIndex <$> filter (filterPair pair) triples)  | pair <- edgePairs faces ]
        where
            triples :: [(Int, Int, Int)]
            triples = halfEdgeTriples faces

            filterPair :: (Int, Int) -> (Int, Int, Int) -> Bool
            filterPair (i, j) (_, h, k) = h == i && k == j

            faceIndex (x, _, _) = x

    makeHeMap :: [IndexedFace] -> HashMap (Int, Int, Int) HalfEdge
    makeHeMap faces =
        let
            edges = edgePairs faces
            faceMap = edgeFacesMap faces
        in
            HashMap.fromList $ do
                pair <- edges
                let faces = faceMap ! pair
                vertex <- [fst pair, snd pair]
                face <- faces
                return $ (
                    (face, fst pair, snd pair)
                    , HalfEdge {
                        previous = -1
                    ,   next = -1
                    ,   vertex = vertex
                    ,   face = face
                    ,   opposite = -1
                    })

    makePrevious :: [IndexedFace] -> HashMap (Int, Int, Int) HalfEdge
    makePrevious faces =
        let
            heMap = makeHeMap faces
            tripleIndices = halfEdgeIndices faces
            indexedFaces = zip [0 ..] faces
        in
            HashMap.fromList $ do
                (index, face) <- indexedFaces
                let edges = faceEdges face
                (previous, edge) <- zip edges (shift edges)
                let triple = (index, fst edge, snd edge)
                let previousTriple = (index, fst previous, snd previous)
                let halfEdge = heMap ! triple
                return (triple, halfEdge { previous = tripleIndices ! previousTriple })

    makeNext :: [IndexedFace] -> HashMap (Int, Int, Int) HalfEdge
    makeNext faces =
        let
            heMap = makePrevious faces
            tripleIndices = halfEdgeIndices faces
            tripleMap = halfEdgeMap faces
            triples = halfEdgeTriples faces
        in
            HashMap.fromList $ do
                triple <- triples
                let halfEdge = heMap ! triple
                let previousTriple = tripleMap ! (previous halfEdge)
                let previousHalfEdge = heMap ! previousTriple
                return (previousTriple, previousHalfEdge { next = tripleIndices ! triple })

    makeOpposite :: [IndexedFace] -> HashMap (Int, Int, Int) HalfEdge
    makeOpposite faces =
        let
            heMap = makeNext faces
            pairs = edgePairs faces
            faceMap = edgeFacesMap faces
            tripleIndices = halfEdgeIndices faces
        in
            HashMap.fromList $ do
                pair <- pairs
                let faceIndices = faceMap ! pair
                let triples = [ (faceIndex, fst pair, snd pair) | faceIndex <- faceIndices ]
                (triple, oppositeTriple) <- zip (shift triples) triples
                let halfEdge = heMap ! triple
                return (triple, halfEdge { opposite = tripleIndices ! oppositeTriple })

    -- FIXME: use mutable arrays to make the mesh
