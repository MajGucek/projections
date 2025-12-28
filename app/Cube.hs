module Cube (
    Cube(..),
    Transformation(..),
    getCubeOriginCoord,
    onVertices,
    getVertexList,
    getFaces,
    applyTransformations,
    localTransform,
    translateCube,
    setTransform,
    rotateCube,
    scaleCube,
    createCube
) where

import MathLib

data Transformation = Transformation {
    translation :: !Vector,
    rotation :: !Quaternion,
    scaling :: !Vector
} deriving Show

data Cube = Cube {
    vertices :: [Vector],
    transform :: !Transformation
} deriving Show

createCube :: Vector -> Vector -> Cube
createCube (Vector sx sy sz) (Vector ex ey ez) =
    Cube {
        vertices = [
            Vector sx sy sz,
            Vector ex sy sz,
            Vector ex ey sz,
            Vector sx ey sz,

            Vector sx sy ez,
            Vector ex sy ez,
            Vector ex ey ez,
            Vector sx ey ez
        ],
        transform = Transformation {
            translation = Vector 0 0 0,
            rotation = Quaternion 1 0 0 0,
            scaling = Vector 1 1 1
        }
    } 

getCubeOriginCoord :: Cube -> Vector
getCubeOriginCoord cube =
    Vector {
        x = 1/8 * foldl (\add v -> add + x v) 0 (vertices cube),
        y = 1/8 * foldl (\add v -> add + y v) 0 (vertices cube),
        z = 1/8 * foldl (\add v -> add + z v) 0 (vertices cube)
    }

onVertices :: (Vector -> Vector) -> Cube -> Cube
onVertices f cube = 
    cube {
        vertices = map f $ vertices cube
    }

getVertexList :: [Vector] -> [Int] -> [Vector]
getVertexList list indexes =
    map (list !!) indexes

getFaces :: Cube -> [[Vector]]
getFaces cube = 
    let v = vertices cube
    in 
    [
        getVertexList v [0, 1, 2, 3], -- bottom
        getVertexList v [4, 5, 6, 7], -- top
        getVertexList v [0, 1, 5, 4], -- back
        getVertexList v [2, 3, 7, 6], -- front
        getVertexList v [1, 2, 6, 5], -- right
        getVertexList v [0, 3, 7, 4]  -- left
    ]


translateCube :: Vector -> Cube -> Cube
translateCube vec_translation cube = 
    onVertices (+++ vec_translation) cube


localTransform :: (Cube -> Cube) -> Cube -> Cube
localTransform f cube =
    let cube_origin_vector = getCubeOriginCoord cube
        cube' = translateCube (opposite cube_origin_vector) cube
        cube'' = f cube'
    in translateCube cube_origin_vector cube'' 


rotateCube :: Quaternion -> Cube -> Cube
rotateCube quat_rotation cube = 
    localTransform (onVertices (quatToPoint . (⟳ quat_rotation) . pointToQuat)) cube

scaleCube :: Vector -> Cube -> Cube
scaleCube vec_scale cube =
    localTransform (onVertices (⛶ vec_scale)) cube

applyTransformations :: Cube -> Cube
applyTransformations cube =
        (
        translateCube (translation (transform cube)) .
        rotateCube (rotation (transform cube)) .
        scaleCube (scaling (transform cube))
        ) 
            cube

setTransform :: Transformation -> Cube -> Cube
setTransform trans cube =
    cube { transform = trans } 

