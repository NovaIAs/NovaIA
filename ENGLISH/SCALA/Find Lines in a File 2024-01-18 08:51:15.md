```scala
// A program that takes two command-line arguments,
// a file name and a search string, and prints the
// line numbers of the lines that contain the search string.

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

// The main function, which takes two arguments:
// - `args(0)`: The file name
// - `args(1)`: The search string
def main(args: Array[String]): Unit = {
  // Check if the command-line arguments were provided.
  if (args.length != 2) {
    println("Usage: scala FindLines <file-name> <search-string>")
    return
  }

  // Get the file name and the search string from the command-line arguments.
  val fileName = args(0)
  val searchString = args(1)

  // Read the file contents into a list of lines.
  val lines = Files.readAllLines(Paths.get(fileName)).asScala

  // Find the line numbers of the lines that contain the search string.
  val lineNumbers = lines.zipWithIndex.filter(_._1.contains(searchString)).map(_._2 + 1)

  // Print the line numbers of the lines that contain the search string.
  for (lineNumber <- lineNumbers) {
    println(lineNumber)
  }
}
```

This code is a Java program that takes two command-line arguments, a file name and a search string, and prints the line numbers of the lines that contain the search string.

The program first checks if the command-line arguments were provided. If they were not, the program prints a usage message and returns.

If the command-line arguments were provided, the program gets the file name and the search string from the command-line arguments.

The program then reads the file contents into a list of lines.

The program then finds the line numbers of the lines that contain the search string.

Finally, the program prints the line numbers of the lines that contain the search string.