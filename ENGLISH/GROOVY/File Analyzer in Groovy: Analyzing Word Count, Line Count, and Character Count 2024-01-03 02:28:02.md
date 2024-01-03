Certainly! Here's a complex and differentiated Groovy code that showcases various programming concepts such as object-oriented programming, exception handling, and file manipulation.

```groovy
import java.nio.file.*

class FileAnalyzer {
    static void analyzeFile(String filePath) {
        try {
            def fileContent = Files.readAllLines(Paths.get(filePath))
            def wordCount = fileContent.join(' ').split(/\s+/).count
            def lineCount = fileContent.size()
            def charCount = fileContent.join().size()

            println "File Analysis Results:"
            println "Word Count: $wordCount"
            println "Line Count: $lineCount"
            println "Character Count: $charCount"
        } catch (NoSuchFileException e) {
            println "File not found: $filePath"
        } catch (IOException e) {
            println "An error occurred while reading the file: $e.message"
        }
    }
}

def filePath = "path/to/your/file.txt"
FileAnalyzer.analyzeFile(filePath)
```

Explanation:
1. The code starts by importing the necessary classes for file manipulation (`java.nio.file.*`).
2. Next, a `FileAnalyzer` class is defined to encapsulate the file analysis logic.
3. Inside the `FileAnalyzer` class, there is a static method `analyzeFile(String filePath)` that takes a file path as input.
4. Within the `analyzeFile` method, the code attempts to read the contents of the file specified by the `filePath` parameter using the `Files.readAllLines()` method. This method reads all lines from the file and returns them as a list of strings.
5. The code then calculates the word count by joining all lines with a space character (`' '`) and splitting the resulting string using a regular expression (`/\s+/`) to separate words. The resulting array is counted using the `count` property.
6. Similarly, the line count is determined by getting the size of the `fileContent` list.
7. The character count is obtained by joining all lines without any delimiter and calculating the size of the resulting string.
8. Finally, the analysis results are printed to the console.
9. The code includes exception handling using `try-catch` blocks. If the file specified by `filePath` does not exist, a `NoSuchFileException` will be caught and an appropriate error message will be printed. If any other I/O error occurs during file reading, an `IOException` will be caught, and the specific error message will be displayed.
10. At the end of the code, the `analyzeFile` method is called with a sample file path (`"path/to/your/file.txt"`). You can replace it with the actual path to the file you want to analyze.

This code reads a file, analyzes its contents, and prints the word count, line count, and character count. It also handles possible exceptions that may occur during file reading.