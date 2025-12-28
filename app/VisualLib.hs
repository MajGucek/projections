module VisualLib (
    circleScale,
    project,
    checkCulling,
    checkFaceCulling,

    window,
    background,
    fColor
) where

import MathLib
import Graphics.Gloss

window :: Display
window = FullScreen --Gloss

background :: Color
background = black --Gloss

fColor :: Picture -> Picture
fColor p = 
    color white p



checkCulling :: Float -> MathLib.Vector -> Bool
checkCulling focalLength v = 
    z v + focalLength > 1

checkFaceCulling :: Float -> [MathLib.Vector] -> Bool
checkFaceCulling focalLength xs =
    any (checkCulling focalLength) xs 

circleScale :: Float
circleScale = 5
project :: Float -> MathLib.Vector -> ((Float, Float), Float)
project focalLength v = 
    let cameraZ = 0
        px = x v * (focalLength / (z v + focalLength)) 
        py = (y v * (focalLength / (z v + focalLength)))
        scale' = circleScale * (focalLength / (z v + cameraZ))
    in ((px, py), scale')