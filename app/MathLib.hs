module MathLib (
    module Math.Vector,
    module Math.Quaternion,
    pointToQuat,
    quatToPoint,
    makeRotationQuat
) where

import Math.Vector
import Math.Quaternion



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

pointToQuat :: Vector -> Quaternion 
pointToQuat (Vector x' y' z') = Quaternion 0 x' y' z'

quatToPoint :: Quaternion -> Vector
quatToPoint (Quaternion _r i' j' k') = Vector {x = i', y = j', z = k'}

