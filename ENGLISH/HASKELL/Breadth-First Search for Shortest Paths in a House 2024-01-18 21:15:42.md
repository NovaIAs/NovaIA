```haskell
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

-- Data types

data House = House {rooms :: [Room]} deriving (Show)
data Room = Room {name :: String, exits :: Set String} deriving (Show)

-- Functions

getRoom :: String -> House -> Maybe Room
getRoom roomName house = find ((== roomName) . name) (rooms house)

canGo :: String -> Room -> Bool
canGo nextRoom currentRoom = nextRoom `member` exits currentRoom

shortestPath :: String -> String -> House -> Maybe [String]
shortestPath startRoom endRoom house =
  let visited = Set.singleton startRoom
      toVisit = [(startRoom, [startRoom])]
  in go toVisit
 where
  go [] = Nothing
  go ((currentRoom, path) : toVisit)
    | currentRoom == endRoom = Just path
    | otherwise =
        let adjacentRooms =
              filter (canGo currentRoom) (rooms house)
                  \\ visited
        in go (adjacentRooms >>= \nextRoom ->
                (nextRoom, nextRoom : path) : toVisit)

findShortestPaths :: House -> [String] -> Set (String, [String])
findShortestPaths house roomNames =
  Set.unions
    [ Set.map (\path -> (startRoom, path))
      (shortestPath startRoom endRoom house)
    | (startRoom, endRoom) <- zip roomNames (tail roomNames)
    ]

-- Main program

main :: IO ()
main = do
  let house =
        House
          [ Room "kitchen" (Set.fromList ["living room", "dining room"]),
            Room "living room" (Set.fromList ["kitchen", "dining room", "study"]),
            Room "dining room" (Set.fromList ["kitchen", "living room", "study"]),
            Room "study" (Set.fromList ["living room", "dining room", "bedroom"]),
            Room "bedroom" (Set.fromList ["study", "bathroom"]),
            Room "bathroom" (Set.fromList ["bedroom"])
          ]
      roomNames = ["kitchen", "living room", "dining room", "study", "bedroom", "bathroom"]
  print $ findShortestPaths house roomNames
```

This code implements a breadth-first search algorithm to find the shortest paths between pairs of rooms in a house. The house is represented as a data type `House` which contains a list of `Room`s. Each `Room` has a name and a set of exits, which are the names of other rooms that can be reached from that room.

The function `getRoom` takes a room name and a house and returns the corresponding `Room` object if it exists, or `Nothing` if it does not.

The function `canGo` takes a room name and a room and returns `True` if there is an exit from the room to the given room, or `False` otherwise.

The function `shortestPath` takes the names of two rooms and a house and returns the shortest path between the two rooms, if one exists. The path is represented as a list of room names, with the first room being the starting room and the last room being the ending room.

The function `findShortestPaths` takes a house and a list of room names and returns a set of pairs of room names and shortest paths between them.

The main function creates a house and a list of room names, and then uses the `findShortestPaths` function to find the shortest paths between all pairs of rooms in the house. The results are then printed to the console.