```haskell
-- Calculating the Mandelbrot set using Haskell
-- This code generates an ASCII representation of the Mandelbrot set, a beautiful fractal that is defined by a simple mathematical equation.

-- Import the necessary libraries
import Control.Monad (mapM_)
import Data.Complex (Complex(..), magnitude)
import Data.List (unfoldr)
import System.Environment (getArgs)

-- Define the Mandelbrot function
mandelbrot :: Complex Double -> Int
mandelbrot c = mandelbrotIter 0 c 255

-- Define the Mandelbrot iteration function
mandelbrotIter :: Int -> Complex Double -> Int -> Int
mandelbrotIter n z maxIter
  | n >= maxIter  = n
  | magnitude z > 2 = n
  | otherwise      = mandelbrotIter (n + 1) (z*z + c) maxIter

-- Generate a list of complex numbers in the desired range
complexGrid :: (Double,Double) -> (Double,Double) -> Int -> [Complex Double]
complexGrid (xmin,xmax) (ymin,ymax) n = unfoldr (Just . nextComplex) (xmin,xmax,ymin,ymax,n)
  where
    nextComplex (x,xmax,y,ymax,n) =
      let x' = (x - xmin) / (xmax - xmin) * fromIntegral n - 0.5
          y' = (y - ymin) / (ymax - ymin) * fromIntegral n - 0.5
          z  = Complex x' y'
      in Just (z, (x + (xmax-xmin)/(n-1),xmax,y,ymax,n))

-- Convert a complex number to a character
complexToChar :: Complex Double -> Char
complexToChar z
  | magnitude z < 2 = ' '
  | otherwise        = ".-=+*#@" !! (mandelbrot z `mod` 9)

-- Convert a grid of complex numbers to a string
gridToString :: [Complex Double] -> String
gridToString grid = unlines (map (map complexToChar) $ chunksOf n grid)

-- Split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs

-- Main function
main :: IO ()
main = do
  -- Get the command-line arguments
  args <- getArgs
  let (xmin,xmax,ymin,ymax,n) = case args of
        [xmin',xmax',ymin',ymax',n'] -> ((read xmin'),(read xmax'),(read ymin'),(read ymax'),(read n'))
        _                           -> ((-2.0,1.0),(-1.5,1.5),500)

  -- Calculate the Mandelbrot set
  let grid = complexGrid (xmin,xmax) (ymin,ymax) n
  let mandelbrotStr = gridToString grid

  -- Print the Mandelbrot set
  putStr mandelbrotStr
```

This code calculates the Mandelbrot set, a beautiful fractal that is defined by a simple mathematical equation. The code first generates a grid of complex numbers in the desired range. Then, it iterates over the grid and calculates the Mandelbrot value for each complex number. The Mandelbrot value is a measure of how quickly the complex number escapes to infinity under repeated application of the Mandelbrot function. Finally, the code converts the grid of Mandelbrot values to a string and prints it to the console.

Here is a breakdown of the code:

* The `mandelbrot` function takes a complex number `c` as input and returns the number of iterations it takes for the complex number to escape to infinity under repeated application of the Mandelbrot function.
* The `mandelbrotIter` function is a helper function that implements the Mandelbrot iteration. It takes the current iteration number, the current complex number, and the maximum number of iterations as input. It returns the number of iterations it takes for the complex number to escape to infinity.
* The `complexGrid` function generates a grid of complex numbers in the desired range. It takes the minimum and maximum values for the real and imaginary parts of the complex numbers, as well as the number of complex numbers to generate, as input. It returns a list of complex numbers.
* The `complexToChar` function converts a complex number to a character. It uses the magnitude of the complex number to determine the character.
* The `gridToString` function converts a grid of complex numbers to a string. It uses the `complexToChar` function to convert each complex number to a character. It then uses the `chunksOf` function to split the grid into chunks of a given size. Finally, it uses the `unlines` function to concatenate the chunks into a single string.
* The `chunksOf` function splits a list into chunks of a given size. It takes the size of the chunks and the list to split as input. It returns a list of lists, where each list is a chunk of the input list.
* The `main` function is the entry point to the program. It gets the command-line arguments, calculates the Mandelbrot set, and prints it to the console.

This code is a good example of how Haskell can be used to write complex and mathematical programs in a concise and elegant way. Haskell's strong support for mathematical operations and its lazy evaluation make it a natural choice for this type of programming.