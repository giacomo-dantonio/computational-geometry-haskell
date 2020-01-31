module Instances where

import Text.ParserCombinators.ReadP
import Obj
import Parse

class ObjLine a where
    addLine :: ObjFile -> a -> ObjFile
    parse :: ReadP a

instance ObjLine ObjVertex where
    addLine file vertex = file { vertices = vertices file ++ [vertex] }
    parse = vertex

instance ObjLine ObjTexture where
    addLine file texture = file { textures = textures file ++ [texture] }
    parse = texture

instance ObjLine ObjNormal where
    addLine file normal = file { normals = normals file ++ [normal] }
    parse = normal
    
instance ObjLine ObjParameter where
    addLine file parameter = file { parameters = parameters file ++ [parameter] }
    parse = parameter

instance ObjLine ObjFace where
    addLine file face = file { faces = faces file ++ [face] }
    parse = face

instance ObjLine Line where
    addLine file line = file { polylines = polylines file ++ [line] }
    parse = objPolyline
