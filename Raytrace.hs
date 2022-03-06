module Raytrace where

toRadians :: Float -> Float
toRadians deg = deg * (pi/180)

toDegrees :: Float -> Float
toDegrees rads = rads * (180/pi)

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

-- Converts a vector to a string delimited by spaces
vecToStr :: Vec -> String
vecToStr = concatMap ((++" ").show.floor)

-- A 4x4 matrix (A single list represented by (x + y*4))
type Mat = [Float]

getMatVal :: Mat -> Int -> Int -> Float
getMatVal m x y = m !! (x+y*4)

-- Matrix-Vector multiplication
-- This assumes the Vec is |3|
matVecMul :: Mat -> Vec -> Vec
matVecMul m v = [rowMul x | x<-three]
    where
        rowMul col = sum [getMatVal m x col * (v!!x) | x<-three]
        three = [0,1,2]

-- Matrix-Matrix multiplication
matMul :: Mat -> Mat -> Mat
matMul a b = [cij x y | y<-four,x<-four]
    where
        cij x y = sum [getMatVal a x n * getMatVal b n y | n <- four]
        four    = [0..3]

matXRotation :: Float -> Mat
matXRotation ang = [
    1, 0, 0, 0,
    0, c,-s, 0,
    0, s, c, 0,
    0, 0, 0, 1 ]
    where
        s = sin $ toRadians ang
        c = cos $ toRadians ang

matYRotation :: Float -> Mat
matYRotation ang = [
    c, 0, s, 0,
    0, 1, 0, 0,
    -s,0, c, 0,
    0, 0, 0, 1 ]
    where
        s = sin $ toRadians ang
        c = cos $ toRadians ang

matZRotation :: Float -> Mat
matZRotation ang = [
    c,-s, 0, 0,
    s, c, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1 ]
    where
        s = sin $ toRadians ang
        c = cos $ toRadians ang

matRotation :: Vec -> Mat
matRotation [x,y,z] = matXRotation x `matMul` matYRotation y `matMul` matZRotation z 
matRotation _       = [] -- should never happen


-- Define different renderable objects
data Object3D =
    Sphere {pos::Vec, radius::Float, color::Vec} |
    Plane {pos::Vec, norm::Vec, color::Vec}
    deriving (Eq, Show)

-- A ray has a starting point and a direction
data Ray = Ray {origin::Vec, dir::Vec} deriving (Eq, Show)

data Camera = Camera {camPos::Vec, camRot::Vec} deriving (Eq,Show)

data Light = Light {lightPos::Vec, lightIntensity::Float}

-- Set of objects and the light source
data World = World [Object3D] [Light]

-- A single ray could intersect with multiple objects.
-- It could also pass through a single object multiple times.
-- This function takes a ray and a list of objects then returns 
--   a list of all intersections and the distance to the intersection with the object.
rayObjectIntersections :: Ray -> World -> [([Float], Object3D)]
rayObjectIntersections r (World objs _) = [(rayIntersection r obj, obj) | obj <- objs, not $ null $ rayIntersection r obj]


-- ray, object, return distances to intersection
-- an empty list means it did not intersect
rayIntersection :: Ray -> Object3D -> [Float]

-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
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

-- https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
rayIntersection r p@Plane {}
    | lDotN == 0 = []
    | otherwise  = filter (>=0) [d]
    where
        lDotN = dir r /./ norm p
        d     = ((pos p /-/ origin r) /./ norm p) / lDotN

{-
--== RUNNING SECTION ==--
-}

dimensions :: (Int, Int)
dimensions = (128, 72)

fieldOfView :: Float
fieldOfView = 90

aspectRatio :: Float
aspectRatio = x / y
    where
        x = fromIntegral $ fst dimensions
        y = fromIntegral $ snd dimensions

defaultCamera :: Camera
defaultCamera = Camera [0,1,0] [-15,0,0] -- up/down, left/right, roll

createRay :: Int -> Int -> Ray
createRay x y = Ray (camPos defaultCamera) direction
    where
        invWidth  = 1 / fromIntegral width
        invHeight = 1 / fromIntegral height
        angle     = tan (pi * 0.5 * fieldOfView / 180)
        xx        = (2*((fromIntegral x+0.5)*invWidth)-1)*angle*aspectRatio
        yy        = (1-2*((fromIntegral y+0.5)*invHeight))*angle
        width     = fst dimensions
        height    = snd dimensions
        direction = matRotation (camRot defaultCamera) `matVecMul` unit [xx,yy,-1]

-- Creates a list of rays based on the dimensions of the image and the fov
generateRayMatrix :: [Ray]
generateRayMatrix = [createRay x y | y <- [0..height-1], x <- [0..width-1]]
    where
        width  = fst dimensions
        height = snd dimensions

getColor :: Ray -> [([Float], Object3D)] -> Vec
getColor r []      = [0,0,0] -- did not intersect
getColor r o@((d, ob):oss)  = color $ closest o (head d) ob -- TEMPORARY
    where
        closest [(dist, obj)] minDist minObj -- only one object
            | minimum dist < minDist = obj
            | otherwise              = minObj
        closest ((dist, obj):xs) minDist minObj 
            | minimum dist < minDist = closest xs (minimum dist) obj
            | otherwise              = closest xs minDist minObj
        closest _ _ _ = undefined -- should never reach here

-- Diffuse colors
getDiffuse :: Ray -> World -> Object3D -> Vec -> Vec
getDiffuse = undefined

main :: IO ()
main = do
    putStrLn "P3"
    putStrLn (show (fst dimensions)++" "++show (snd dimensions))
    putStrLn "255"
    putStrLn $ concatMap (\x -> vecToStr $ getColor x (rayObjectIntersections x testWorld)) generateRayMatrix


{-
--== TESTING SECTION ==--
-}

testPlane :: Object3D
testPlane = Plane [0,0,0] [0,1,0] [120,0,120]

testPlane2 :: Object3D
testPlane2 = Plane [-5,0,-5] [-1,-1,-0.5] [0,0,120]

testSphere :: Object3D
testSphere = Sphere [2,0.5,-2] 1 [0,120,120]

testSphere2 :: Object3D
testSphere2 = Sphere [0,0,-8] 2 [0,120,0]

testSphere3 :: Object3D
testSphere3 = Sphere [-5,3,-7] 2 [120,0,0]

testLight :: Light
testLight = Light [0,100,0] 1

testWorld :: World
testWorld = World [testPlane,testSphere,testSphere2,testPlane2,testSphere3] [testLight]


testRay :: Ray
testRay = Ray [0,10,0] [0,-1,0]

test :: [([Float], Object3D)]
test = rayObjectIntersections testRay testWorld
