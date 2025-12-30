module VisualLib (
    project,
    checkCulling,
    checkFaceCulling,
    clampFocalLength,

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
fColor = 
    color white

clampFocalLength :: Float -> Float
clampFocalLength = max 100

checkCulling :: Float -> MathLib.Vector -> Bool
checkCulling focalLength v = 
    z v + clampFocalLength focalLength > 1

checkFaceCulling :: Float -> [MathLib.Vector] -> Bool
checkFaceCulling focalLength =
    any (checkCulling $ clampFocalLength focalLength)

project :: Float -> MathLib.Vector -> (Float, Float)
project focalLength' v = 
    let focalLength = clampFocalLength focalLength'
        px = x v * (focalLength / (z v + focalLength)) 
        py = (y v * (focalLength / (z v + focalLength)))
    in (px, py)