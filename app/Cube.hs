module Cube (
    Cube(..),
    Transformation(..),
    onVertices,
    localTransform,
    createCube
) where

import MathLib



data Cube = Cube {
    vertices :: [Vector],
    transform :: !Transformation
} deriving Show

instance Shape Cube where
    getVertices = vertices
    setVertices vertices' cube = cube { vertices = vertices' } 
    getTransform = transform
    setTransform transform' cube = cube { transform = transform' } 
    getLocalOrigin cube = 
        Vector {
            x = 1/8 * foldl (\add v -> add + x v) 0 (vertices cube),
            y = 1/8 * foldl (\add v -> add + y v) 0 (vertices cube),
            z = 1/8 * foldl (\add v -> add + z v) 0 (vertices cube)
        }
    toMesh cube = 
        let v = getVertices cube
        in --Order: CCW, first Bottom then top, start from left bottom corner in top-down view
            map toTriangle [
                map (v !!) [0, 1, 2], -- Bottom
                map (v !!) [0, 2, 3],

                map (v !!) [4, 5, 6], -- Top
                map (v !!) [4, 6, 7],

                map (v !!) [0, 1, 5], -- Back
                map (v !!) [0, 5, 4],

                map (v !!) [2, 3, 7], -- Front
                map (v !!) [2, 7, 6],

                map (v !!) [1, 2, 6], -- Right
                map (v !!) [1, 6, 5],

                map (v !!) [0, 3, 7], -- Left
                map (v !!) [0, 7, 4]
            ]
  


createCube :: (Vector, Vector) -> Cube
createCube (Vector sx sy sz, Vector ex ey ez) =
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













