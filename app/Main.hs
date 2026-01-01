{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.IO.Game

import MathLib
import VisualLib
import Cube
import Pyramid


data WorldState = WorldState {
    shapes :: [AnyShape],
    focalLength :: Float,
    time :: Float,
    name :: String
}

initialState :: WorldState
initialState = WorldState {
    shapes = [
        AnyShape (createCube (Vector (-50) (-50) 50, Vector 50 50 150)),
        AnyShape (createCube (Vector (-50) (-50) 50, Vector 50 50 150)),
        AnyShape (createPyramid (Vector 0 0 100, 100, 100))
    ],
    focalLength = 1000,
    time = 0,
    name = "ID"
}


render :: WorldState -> Picture
render state =
    pictures
        $ map (fColor . polygon . map (project $ focalLength state))
        $ concatMap 
        (
            filter (checkFaceCulling $ focalLength state) .
            map fromTriangle .
            concreteShape (
                toMesh .
                applyTransformations
                )
        ) (shapes state)
        {-
            (withShape $
            filter (checkFaceCulling $ focalLength state) .
            map fromTriangle .
            toMesh .
            applyTransformations
            ) (shapes state)
-}




update :: Float -> WorldState -> WorldState
update dt state =
    let speed = pi / 2
        t = time state + dt
        transformations = [
            Transformation {
                translation = Vector (- (500 * cos t)) (500 * sin t) 0,
                rotation = makeRotationQuat (speed * t) (Vector 0 1 0),
                scaling = Vector 1 1 1
            },
            Transformation {
                translation = Vector (200 * cos t) (200 * sin t) (600 * sin t),
                rotation = makeRotationQuat (speed * t) (Vector 1 0 0),
                scaling = Vector (2 + cos t) 1 1
            },
            Transformation {
                translation = Vector 0 0 (400 * sin t),
                rotation = makeRotationQuat (speed * t) (Vector 1 0.5 1),
                scaling = Vector 1 1 1
            }
            ]
    in state {
        --setTransform :: Transformation -> a -> a
        shapes = 
            zipWith 
                (\transform shape -> updateShape (setTransform transform) shape)
                --Ok so, lambda takes current transformation and shape
                -- and does setTransform on concrete type (withShape)
                -- to current_transform and then rewraps back into AnyShape
                -- AnyShape . setTransform
                transformations (shapes state),
        time = t
    }

handleInput :: Event -> WorldState -> WorldState
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
    state {
        focalLength = clampFocalLength $ focalLength state + 100
    }
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state =
    state {
        focalLength = clampFocalLength $ focalLength state - 100
    }
handleInput _ state = state

fps :: Int
fps = 240
main :: IO ()
main = play window background fps initialState render handleInput update