module Math.Vector (
    Vector(..),
    opposite,
    (+++),
    (~~~),
    (⛶),
    normalize
) where

data Vector = Vector {
    x :: Float,
    y :: Float,
    z :: Float
} deriving Show



opposite :: Vector -> Vector
opposite v = Vector { x = -(x v), y = -(y v), z = -(z v) } 

infixl 6 +++
(+++) :: Vector -> Vector -> Vector
(Vector p3_x p3_y p3_z) +++ (Vector tx ty tz) = 
    Vector (p3_x + tx) (p3_y + ty) (p3_z + tz)

infixl 6 ~~~
(~~~) :: Vector -> Vector -> Vector
p ~~~ t = p +++ opposite t

infixl 7 ⛶
(⛶) :: Vector -> Vector -> Vector
(Vector p3_x p3_y p3_z) ⛶ (Vector sx sy sz) =
    Vector (p3_x * sx) (p3_y * sy) (p3_z * sz)


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