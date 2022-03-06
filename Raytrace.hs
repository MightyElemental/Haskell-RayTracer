module Raytrace where

toRadians :: Float -> Float
toRadians deg = deg * (pi/180)

toDegrees :: Float -> Float
toDegrees rads = rads * (180/pi)

{-
--== VECTOR DEFINITION ==--
-}

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

vecTo :: Vec -> Vec -> Vec
vecTo a b = zipWith (-) b a

negVec :: Vec -> Vec
negVec = map negate

zeroVec :: Vec
zeroVec = repeat 0

sumVecs :: [Vec] -> Vec
sumVecs = foldr (/+/) zeroVec

-- Converts a vector to a string delimited by spaces
vecToStr :: Vec -> String
vecToStr = concatMap ((++" ").show.floor)

{-
--== MATRIX DEFINITION ==--
-}

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

getHitPos :: Ray -> Float -> Vec
getHitPos (Ray o dir) dist = o /+/ (dist /*/ dir)

data Camera = Camera {camPos::Vec, camRot::Vec} deriving (Eq,Show)

data Light = Light {lightPos::Vec, lightIntensity::Float, lightColor::Vec}

-- Set of objects and light sources
data World = World [Object3D] [Light]

-- A single ray could intersect with multiple objects.
-- It could also pass through a single object multiple times.
-- This function takes a ray and a list of objects then returns 
--   a list of all intersections and the distance to the intersection with the object.
rayObjectIntersections :: Ray -> World -> [([Float], Object3D)]
rayObjectIntersections r (World objs _) = [(rayIntersection r obj, obj) | obj <- objs, not $ null $ rayIntersection r obj]

rayObjNormal :: Ray -> Vec -> Object3D -> Vec
rayObjNormal (Ray _ rayDir) hitPos s@Sphere {} =
    pos s `vecTo` hitPos
rayObjNormal (Ray _ rayDir) hitPos p@Plane {}
    | rayDir /./ norm p < 0 = norm p -- test for which face the ray hit
    | otherwise = negVec $ norm p


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
dimensions = (1280, 720)

fieldOfView :: Float
fieldOfView = 90

aspectRatio :: Float
aspectRatio = x / y
    where
        x = fromIntegral $ fst dimensions
        y = fromIntegral $ snd dimensions

defaultCamera :: Camera
defaultCamera = Camera [0,1,0.1] [-10,0,0] -- up/down, left/right, roll

-- The multiplier of brightness in complete darkess
ambientCoeff :: Float
ambientCoeff = 0.5

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

getColor :: Ray -> World -> Vec
getColor r w@(World objs lights)
    | null objIntersections = [0,0,0] -- did not intersect
    | otherwise = getDiffuse r w (snd closestObj) (getHitPos r (fst closestObj))
    where
        objIntersections = rayObjectIntersections r w
        closestObj = getClosestObj objIntersections (head $ fst (head objIntersections)) (snd (head objIntersections))

        -- getColor r   = color $ closest o (head d) ob -- TEMPORARY
        

getClosestObj :: [([Float], Object3D)] -> Float -> Object3D -> (Float, Object3D)
getClosestObj [(dist, obj)] minDist minObj -- only one object
    | minimum dist < minDist = (minimum dist, obj)
    | otherwise              = (minDist, minObj)
getClosestObj ((dist, obj):xs) minDist minObj
    | minimum dist < minDist = getClosestObj xs (minimum dist) obj
    | otherwise              = getClosestObj xs minDist minObj
getClosestObj _ _ _ = undefined -- should never reach here

-- Diffuse colors
getDiffuse :: Ray -> World -> Object3D -> Vec -> Vec
getDiffuse r@(Ray _ rayDir) w@(World _ lights) obj hitPos = baseColor /+/ finalLightTotal
    where
        baseColor = ambientCoeff /*/ color obj
        lvec l = unit (hitPos `vecTo` lightPos l) -- unit vector from hit to light
        shadowRay l = Ray (hitPos /+/ (0.01 /*/ lvec l)) (lvec l) -- may need to test for performance
        isNotInShadow l = null $ rayObjectIntersections (shadowRay l) w -- is another object between obj and light
        objLightDotPod l = unit (rayObjNormal r hitPos obj) /./ lvec l -- dot product between object normal and light
        lightCount = fromIntegral (length lights)
        lightAddition l = (lightIntensity l * objLightDotPod l) /*/ lightColor l -- how much color to add to the pixel
        totalLightAddition = sumVecs [lightAddition l | l<-lights, isNotInShadow l, objLightDotPod l > 0] -- all lights added together
        finalLightTotal = ((1-ambientCoeff) / lightCount ) /*/ totalLightAddition

main :: IO ()
main = do
    putStrLn "P3"
    putStrLn (show (fst dimensions)++" "++show (snd dimensions))
    putStrLn "255"
    putStrLn $ concatMap (\x -> vecToStr $ getColor x testWorld) generateRayMatrix


{-
--== TESTING SECTION ==--
-}

testPlane :: Object3D
testPlane = Plane [0,0,0] [0,1,0] [120,0,120]

testPlane2 :: Object3D
testPlane2 = Plane [-5,0,-5] [-1,-1.2,-0.5] [0,0,120]

testSphere :: Object3D
testSphere = Sphere [2,0.5,-2] 1 [0,120,120]

testSphere2 :: Object3D
testSphere2 = Sphere [2,5,-8] 2 [0,120,0]

testSphere3 :: Object3D
testSphere3 = Sphere [-5,3,-7] 2 [120,0,0]

testLight :: Light
testLight = Light [0,100,0] 1 [255,255,255]

testWorld :: World
testWorld = World [testPlane,testSphere,testSphere2,testPlane2,testSphere3] [testLight]