```haskell
import Data.List (nub, sort)
import Data.Ord (comparing)

-- Define the type of a point in 3D space
data Point3D = Point3D {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show)

-- Define a function to calculate the distance between two points
distance :: Point3D -> Point3D -> Double
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
    sqrt $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2

-- Define a function to find the closest pair of points in a list of points
closestPair :: [Point3D] -> (Point3D, Point3D, Double)
closestPair points =
    let sortedPoints = sort points
        distances = [(p1, p2, distance p1 p2) | p1 <- sortedPoints, p2 <- sortedPoints]
        minDistance = minimumBy (comparing snd) distances
    in minDistance

-- Define a function to find all the pairs of points in a list that are within a given distance of each other
pairsWithinDistance :: Double -> [Point3D] -> [(Point3D, Point3D)]
pairsWithinDistance distance points =
    let distances = [(p1, p2, distance p1 p2) | p1 <- points, p2 <- points, p1 /= p2]
        filteredDistances = filter ((<=) distance . snd) distances
    in map (\(p1, _, _) -> (p1, snd p1)) filteredDistances

-- Main function
main :: IO ()
main = do
    -- Read the list of points from a file
    points <- map (Point3D <$> map read . words) . lines <$> readFile "points.txt"

    -- Find the closest pair of points
    let (p1, p2, closestDistance) = closestPair points

    -- Print the closest pair of points and the distance between them
    putStrLn $ "Closest pair of points: " ++ show (p1, p2)
    putStrLn $ "Distance between them: " ++ show closestDistance

    -- Find all the pairs of points that are within 10 units of each other
    let closePairs = pairsWithinDistance 10 points

    -- Print the pairs of points that are within 10 units of each other
    putStrLn "Pairs of points within 10 units of each other:"
    mapM_ print closePairs
```

Explanation:

* The `Point3D` data type represents a point in 3D space. It has three fields: `x`, `y`, and `z`, which are the coordinates of the point.
* The `distance` function calculates the distance between two points.
* The `closestPair` function finds the closest pair of points in a list of points. It first sorts the points and then calculates the distance between each pair of points. The pair with the smallest distance is returned.
* The `pairsWithinDistance` function finds all the pairs of points in a list that are within a given distance of each other. It first calculates the distance between each pair of points and then filters out the pairs that are not within the given distance.
* The `main` function reads the list of points from a file, finds the closest pair of points, and prints the closest pair of points and the distance between them. It also finds all the pairs of points that are within 10 units of each other and prints them.