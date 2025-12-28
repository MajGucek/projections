module Math.Quaternion (
    Quaternion(..),
    conjugate,
    (***),
    (⟳)
) where

data Quaternion = Quaternion {
    r :: Float,
    i :: Float,
    j :: Float,
    k :: Float
} deriving Show




conjugate :: Quaternion -> Quaternion
conjugate q = q { i = -(i q), j = -(j q), k = -(k q)}

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