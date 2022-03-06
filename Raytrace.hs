module Raytrace where

-- Define vector type
type Vec = [Float]

-- Vector addition
(/+/) :: Vec -> Vec -> Vec
(/+/) = zipWith (+)

-- Vector subtraction
(/-/) :: Vec -> Vec -> Vec
(/-/) = zipWith (-)

-- Vector-Scalar multiplication
(/*/) :: Float -> Vec -> Vec
(/*/) = map . (*)

-- Dot product of two vectors
(/./) :: Vec -> Vec -> Float
(/./) a b = sum $ zipWith (*) a b

-- Vector length
vecLen :: Vec -> Float
vecLen = sqrt . sum . map (^2)

-- Unit vector
unit :: Vec -> Vec
unit a = map (/length) a
    where length = vecLen a



-- Define different renderable objects
data Object3D = Sphere {pos::Vec, radius::Float, color::Vec} | Plane {pos::Vec, norm::Vec, color::Vec}

-- A ray has a starting point and a direction
data Ray = Ray {origin::Vec, dir::Vec} deriving (Eq, Show)

-- A single ray could intersect with multiple objects.
-- It could also pass through a single object multiple times.
-- This function takes a ray and a list of objects then returns 
--   a list of all intersections and the distance to the intersection with the object.
rayObjectIntersections :: Ray -> [Object3D] -> [([Float], Object3D)]
rayObjectIntersections = undefined


-- ray, object, return distances to intersection
-- an empty list means it did not intersect
rayIntersection :: Ray -> Object3D -> [Float]
rayIntersection r s@Sphere {}
    | nabla < 0  = []
    | nabla == 0 = filter (>=0) [dfh]
    | otherwise  = filter (>=0) [negDist, posDist]
    where 
        nabla   = (uv /./ oSubC)^2 - vecLen oSubC^2 + radius s^2
        uv      = unit $ dir r
        oSubC   = origin r /-/ pos s
        dfh     = -(uv /./ oSubC)
        posDist = dfh + sqrt nabla
        negDist = dfh - sqrt nabla

rayIntersection r p@Plane {} = undefined


{-
--== TESTING SECTION ==--
-}

testSphere :: Object3D
testSphere = Sphere [1,1,1] 5 [0,1,1]

testRay :: Ray
testRay = Ray [0,0,0] [1,0,0]

test :: Vec
test = rayIntersection testRay testSphere