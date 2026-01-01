{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module MathLib (
    module Math.Vector,
    module Math.Quaternion,
    Transformation(..),
    Triangle,
    Shape(..),
    AnyShape(..),
    updateShape,
    concreteShape,
    pointToQuat,
    quatToPoint,
    makeRotationQuat,
    toTriangle,
    fromTriangle
) where

import Math.Vector
import Math.Quaternion

data Transformation = Transformation {
    translation :: !Vector,
    rotation :: !Quaternion,
    scaling :: !Vector
} deriving Show

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

toTriangle :: [Vector] -> Triangle
toTriangle (v1:v2:v3:_) = (v1, v2, v3)
toTriangle _ = error "Vector has less then 3 elements"

fromTriangle :: Triangle -> [Vector]
fromTriangle (v1, v2, v3) = [v1, v2, v3]


type Triangle = (Vector, Vector, Vector)

data AnyShape = forall a. Shape a => AnyShape a

updateShape :: (forall a. Shape a => a -> a) -> AnyShape -> AnyShape
updateShape f (AnyShape s) = AnyShape (f s)

concreteShape :: (forall a. Shape a => a -> b) -> AnyShape -> b
concreteShape f (AnyShape s) = f s

class Shape a where
    onVertices :: (Vector -> Vector) -> a -> a
    onVertices f shape = 
        setVertices (map f $ getVertices shape) shape 
    applyTransformations :: a -> a
    applyTransformations shape = 
        (
        translateShape (translation (getTransform shape)) .
        rotateShape (rotation (getTransform shape)) .
        scaleShape (scaling (getTransform shape))
        ) 
            shape
    translateShape :: Vector -> a -> a
    translateShape vec_translation = 
        onVertices (+++ vec_translation)

    localTransform :: (a -> a) -> a -> a
    localTransform f shape =
        let origin_vector = getLocalOrigin shape
            shape' = translateShape (opposite origin_vector) shape
            shape'' = f shape'
        in translateShape origin_vector shape'' 

    rotateShape :: Quaternion -> a -> a
    rotateShape quat_rotation = 
        localTransform (onVertices (quatToPoint . (⟳ quat_rotation) . pointToQuat)) 

    scaleShape :: Vector -> a -> a
    scaleShape vec_scale =
        localTransform (onVertices (⛶ vec_scale)) 


    getVertices :: a -> [Vector]
    setVertices :: [Vector] -> a -> a
    getTransform :: a -> Transformation
    setTransform :: Transformation -> a -> a
    toMesh :: a -> [Triangle]
    getLocalOrigin :: a -> Vector