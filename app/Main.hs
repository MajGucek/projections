{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game (Event)

import MathLib
import VisualLib
import Cube
    


data WorldState = WorldState {
    cubes :: [Cube],
    focalLength :: Float,
    time :: Float,
    name :: String
} deriving Show

initialState :: WorldState
initialState = WorldState {
    cubes = [
        createCube (Vector (-50) (-50) 50) (Vector 50 50 150)
    ],
    focalLength = 1000,
    time = 0,
    name = "ID"
}


render :: WorldState -> Picture
render state = 
        {- 
        for each cube applyTransf
        then getFaces for each cube :t [ [[Vector]] ]
            for each face is [Vector], cube has 6 faces, and i have n-cubes so triple vector
        then for each face checkCulling, then make it a polygon and color it
        at the end collect and give into picture
        -}
        let t = concatMap (getFaces . applyTransformations) (cubes state) -- :t [[Vector]]
            culled = filter (checkFaceCulling $ focalLength state) t -- :t [[Vector]]
        in pictures $
            map (fColor . polygon . map (fst . project (focalLength state))) culled

    

update :: Float -> WorldState -> WorldState
update dt state = 
    let speed = pi / 2
        t = time state + dt
        transformation = Transformation {
            translation = Vector (500 * sin t) (500 * cos t) 0,
            rotation = makeRotationQuat (speed * dt) (Vector 0 1 0),
            scaling = Vector 1 1 1
        }
    in state {
        cubes = map (setTransform transformation) (cubes state),
        time = t
    }

handleInput :: Event -> WorldState -> WorldState
handleInput _event state = state



fps :: Int
fps = 165
main :: IO ()
main = play window background fps initialState render handleInput update