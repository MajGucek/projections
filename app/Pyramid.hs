module Pyramid (
    Pyramid(..),
    Transformation(..),
    onVertices,
    localTransform,
    createPyramid
) where

import MathLib


data Pyramid = Pyramid {
    vertices :: [Vector],
    transform :: !Transformation
}

instance Shape Pyramid where
    getVertices = vertices
    setVertices vertices' pyramid = pyramid { vertices = vertices' } 
    getTransform = transform
    setTransform transform' pyramid = pyramid { transform = transform' } 

    getLocalOrigin pyramid = 
        Vector {
            x = 1/4 * foldl (\add v -> add + x v) 0 (vertices pyramid),
            y = 1/4 * foldl (\add v -> add + y v) 0 (vertices pyramid),
            z = 1/4 * foldl (\add v -> add + z v) 0 (vertices pyramid)
        }
    
    toMesh pyramid = 
        let v = vertices pyramid
        in map toTriangle [
            map (v !!) [0, 1, 2],
            map (v !!) [0, 1, 3],
            map (v !!) [1, 2, 3],
            map (v !!) [0, 2, 3]
        ]



createPyramid :: (Vector, Float, Float) -> Pyramid
createPyramid (Vector _x _y _z, radius, height) = 
    let angle = 2 * pi / 3
    in Pyramid {
        vertices = [
            Vector (_x + radius) _y _z,
            Vector (_x + radius * cos angle) _y (_z + radius * sin angle),
            Vector (_x + radius * cos (2 * angle)) _y (_z + radius * sin (2 * angle)),
            Vector _x (_y + height) _z
        ],
        transform = Transformation {
            translation = Vector 0 0 0,
            rotation = Quaternion 1 0 0 0,
            scaling = Vector 1 1 1
        }
    }