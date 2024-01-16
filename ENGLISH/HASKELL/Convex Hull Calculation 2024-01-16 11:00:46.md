```haskell
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)

-- Data types

-- A point in 2D space
data Point = Point Double Double
  deriving (Eq, Ord)

-- A line segment
data Segment = Segment Point Point
  deriving (Eq, Ord)

-- A polygon
data Polygon = Polygon [Point]
  deriving (Eq, Ord)

-- Functions

-- Calculate the intersection point of two line segments
intersection :: Segment -> Segment -> Maybe Point
intersection (Segment p1 p2) (Segment q1 q2) =
  let
    a = p2 - p1
    b = q2 - q1
    c = p1 - q1
    d = (a `cross` b) / (b `cross` c)
  in if d >= 0 && d <= 1 then Just (p1 + (d * a)) else Nothing

-- Calculate the cross product of two vectors
cross :: Point -> Point -> Double
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- Calculate the area of a polygon
area :: Polygon -> Double
area (Polygon points) =
  let
    n = length points
    points' = points ++ [points !! 0]
  in abs (sum [x * y' - x' * y | (x, y, x', y') <- zip points' (tail points')]) / 2

-- Calculate the perimeter of a polygon
perimeter :: Polygon -> Double
perimeter (Polygon points) =
  let
    n = length points
    points' = points ++ [points !! 0]
  in sum [distance p p' | (p, p') <- zip points points']

-- Calculate the distance between two points
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- Find the convex hull of a set of points
convexHull :: [Point] -> Polygon
convexHull points =
  let
    sortedPoints = sort points
    upperHull = upperConvexHull sortedPoints
    lowerHull = reverse (upperConvexHull (reverse sortedPoints))
  in Polygon (upperHull ++ lowerHull)

-- Find the upper convex hull of a set of points
upperConvexHull :: [Point] -> [Point]
upperConvexHull points =
  let
    stack = []
  in foldl addPoint stack points

-- Add a point to the convex hull stack
addPoint :: [Point] -> Point -> [Point]
addPoint stack point =
  let
    n = length stack
  in if n < 2 then stack ++ [point]
     else if ccw (stack !! (n - 2)) (stack !! (n - 1)) point then stack ++ [point]
          else addPoint (init stack) point

-- Check if three points are in counterclockwise order
ccw :: Point -> Point -> Point -> Bool
ccw (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  let
    a = (x2 - x1) * (y3 - y1)
    b = (x3 - x2) * (y1 - y2)
  in a - b > 0

-- Main function

main :: IO ()
main = do
  let
    points = [
      Point 0 0,
      Point 1 0,
      Point 1 1,
      Point 0 1,
      Point 0.5 0.5
    ]
    polygon = convexHull points
  printf "Area: %.2f\n" (area polygon)
  printf "Perimeter: %.2f\n" (perimeter polygon)
```

This code calculates the convex hull of a set of points, which is the smallest convex polygon that contains all of the points. The code also calculates the area and perimeter of the convex hull.

The code uses the following functions:

* `intersection`: Calculates the intersection point of two line segments.
* `cross`: Calculates the cross product of two vectors.
* `area`: Calculates the area of a polygon.
* `perimeter`: Calculates the perimeter of a polygon.
* `distance`: Calculates the distance between two points.
* `convexHull`: Finds the convex hull of a set of points.
* `upperConvexHull`: Finds the upper convex hull of a set of points.
* `addPoint`: Adds a point to the convex hull stack.
* `ccw`: Checks if three points are in counterclockwise order.

The main function creates a set of points and then calls the `convexHull` function to calculate the convex hull of the points. The main function then prints the area and perimeter of the convex hull.