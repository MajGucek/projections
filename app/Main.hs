{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort (ViewPort)

window :: Display
window = FullScreen --Gloss

background :: Color
background = black --Gloss

fColor :: Picture -> Picture
fColor p = 
    color white p


focalLength :: Float
focalLength = 200

circleScale :: Float
circleScale = 5


project :: Vector -> (Point, Float)
project v = 
    let cameraZ = 200
        px = x v * (focalLength / (z v + focalLength)) 
        py = (y v * (focalLength / (z v + focalLength)))
        scale = circleScale * (focalLength / (z v + cameraZ))
    in ((px, py), scale)

simpleProject :: Vector -> Point
simpleProject v = fst $ project v

data Quaternion = Quaternion {
    r :: Float,
    i :: Float,
    j :: Float,
    k :: Float
} deriving Show

pointToQuat :: Vector -> Quaternion 
pointToQuat (Vector x y z) = Quaternion 0 x y z

quatToPoint :: Quaternion -> Vector
quatToPoint (Quaternion _r i j k) = Vector {x = i, y = j, z = k}


conjugate :: Quaternion -> Quaternion
conjugate q = q { i = -(i q), j = -(j q), k = -(k q)}

data Vector = Vector {
    x :: Float,
    y :: Float,
    z :: Float
} deriving Show

opposite :: Vector -> Vector
opposite v = Vector { x = -(x v), y = -(y v), z = -(z v) } 

newtype Cube = Cube {
    vertices :: [Vector]
} deriving Show

getCubeOriginCoord :: Cube -> Vector
getCubeOriginCoord cube =
    Vector {
        x = 1/8 * foldl (\add v -> add + x v) 0 (vertices cube),
        y = 1/8 * foldl (\add v -> add + y v) 0 (vertices cube),
        z = 1/8 * foldl (\add v -> add + z v) 0 (vertices cube)
    }

onVertices :: (Vector -> Vector) -> Cube -> Cube
onVertices f cube = 
    Cube {
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

infixl 6 +++
(+++) :: Vector -> Vector -> Vector
(Vector p3_x p3_y p3_z) +++ (Vector tx ty tz) = 
    Vector (p3_x + tx) (p3_y + ty) (p3_z + tz)

infixl 7 ⛶
(⛶) :: Vector -> Vector -> Vector
(Vector p3_x p3_y p3_z) ⛶ (Vector sx sy sz) =
    Vector (p3_x * sx) (p3_y * sy) (p3_z * sz)

infixl 7 ***
(***) :: Quaternion -> Quaternion -> Quaternion
(Quaternion r1 i1 j1 k1) *** (Quaternion r2 i2 j2 k2) =
    Quaternion {
        r = r1*r2 - i1*i2 - j1*j2 - k1*k2,
        i = r1*i2 + i1*r2 + j1*k2 - k1*j2,
        j = r1*j2 - i1*k2 + j1*r2 + k1*i2,
        k = r1*k2 + i1*j2 - j1*i2 + k1*r2
    }

infixl 7 ⟳
(⟳) :: Quaternion -> Quaternion -> Quaternion
p ⟳ q = q *** p *** conjugate q


translateCube :: Vector -> Cube -> Cube
translateCube translation cube = 
    onVertices (+++ translation) cube


localTransform :: (Cube -> Cube) -> Cube -> Cube
localTransform f cube =
    let cube_origin_vector = getCubeOriginCoord cube
        cube' = translateCube (opposite cube_origin_vector) cube
        cube'' = f cube'
    in translateCube cube_origin_vector cube'' 


rotateCube :: Quaternion -> Cube -> Cube
rotateCube rotation cube = 
    localTransform (onVertices (quatToPoint . (⟳ rotation) . pointToQuat)) cube

scaleCube :: Vector -> Cube -> Cube
scaleCube scale cube =
    localTransform (onVertices (⛶ scale)) cube


makeRotationQuat :: Float -> Vector -> Quaternion
makeRotationQuat angle v =
    let v' = normalize v
        s = sin(angle / 2)
    in Quaternion {
        r = cos(angle / 2),
        i = s * x v',
        j = s * y v',
        k = s * z v'
    }

normalize :: Vector -> Vector
normalize v = 
    let norm = sqrt(x v ^ 2 + y v ^ 2 + z v ^ 2)
    in if norm < 1e-6
        then Vector 0 0 1
        else 
            Vector {
                x = x v / norm,
                y = y v / norm,
                z = z v / norm
            }


    


data WorldState = WorldState {
    myCube :: Cube,
    translation :: Vector,
    rotation :: Quaternion,
    scaling :: Vector,
    time :: Float,
    name :: String
} deriving Show

initialState :: WorldState
initialState = WorldState {
    myCube = Cube {
        vertices = [
            Vector { x = -50, y = -50, z = 50 },
            Vector { x = 50, y = -50, z = 50 },
            Vector { x = 50, y = 50, z = 50 },
            Vector { x = -50, y = 50, z =  50 },
            Vector { x = -50, y = -50, z = 150 },
            Vector { x = 50, y = -50, z = 150 },
            Vector { x = 50, y = 50, z = 150 },
            Vector { x = -50, y = 50, z = 150 }
        ]
    },
    translation = Vector 0 0 0,
    rotation = Quaternion 1 0 0 0,
    scaling = Vector 1 1 1,
    time = 0,
    name = "ID"
}

checkCulling :: Vector -> Bool
checkCulling v = 
    z v + focalLength > 1

applyTransformations :: WorldState -> Cube
applyTransformations state =
        (
        translateCube (translation state) .
        rotateCube (rotation state) .
        scaleCube (scaling state)
        ) 
            (myCube state)




render :: WorldState -> Picture
render state =
    let faces = getFaces $ applyTransformations state
        culledFaces = filter (all checkCulling) faces
    in pictures $
        map (fColor . polygon . map simpleProject) culledFaces
    


makeStep :: Float -> WorldState -> WorldState
makeStep dt state = 
    let speed = pi / 2
        t = time state + dt
        deltaRotation = makeRotationQuat (speed * dt) (Vector 0 1 0)
        deltaTranslate = Vector (sin t) 0 0
        deltaScale = Vector 0 0 0
    in state {
        translation = deltaTranslate +++ translation state,
        rotation = deltaRotation *** rotation state,
        scaling = deltaScale +++ scaling state,
        time = t
    }
    
fps :: Int
fps = 165

main :: IO ()
main = simulate window background fps initialState render update


update :: ViewPort -> Float -> WorldState -> WorldState
update _ = makeStep