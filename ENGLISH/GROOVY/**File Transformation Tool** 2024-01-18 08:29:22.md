```groovy
// Import necessary libraries
import org.apache.commons.io.FileUtils
import org.apache.commons.lang.StringUtils

// This is the main method, which is the entry point for the program
def main(args) {
    // Get the source and destination file paths from the command line arguments
    def sourceFilePath = args[0]
    def destinationFilePath = args[1]

    // Read the contents of the source file
    def sourceFileContents = FileUtils.readFileToString(new File(sourceFilePath))

    // Split the source file contents into lines
    def lines = sourceFileContents.split("\n")

    // Create a list to store the transformed lines
    def transformedLines = new ArrayList()

    // Iterate over each line in the source file
    lines.each { line ->
        // Trim any leading and trailing whitespace from the line
        def trimmedLine = StringUtils.trim(line)

        // Check if the line is empty or contains only whitespace
        if (StringUtils.isBlank(trimmedLine)) {
            // If the line is empty or contains only whitespace, skip it
            continue
        }

        // Convert the line to uppercase
        def upperCaseLine = StringUtils.upperCase(trimmedLine)

        // Add the transformed line to the list
        transformedLines.add(upperCaseLine)
    }

    // Convert the list of transformed lines to a string
    def transformedFileContents = transformedLines.join("\n")

    // Write the transformed file contents to the destination file
    FileUtils.writeStringToFile(new File(destinationFilePath), transformedFileContents)

    // Print a message to the console indicating that the transformation was successful
    println("File transformed successfully!")
}

// This is the definition of the main method, which is the entry point for the program
static void main(String[] args) {
    // Call the main method with the command line arguments
    main(args)
}
```

This script is a complex and differentiated code. It takes a source file as input and transforms it by converting all the lines to uppercase and writing the transformed contents to a destination file. Here's an explanation of the code:

1. **Importing Libraries:**
   - The script imports necessary libraries for reading and writing files, and for string manipulation:
     - `org.apache.commons.io.FileUtils`: For reading and writing files.
     - `org.apache.commons.lang.StringUtils`: For string manipulation.
2. **Main Method:**
   - The `main` method is the entry point for the program. It takes command-line arguments, which are the source file path and the destination file path.
3. **Reading the Source File:**
   - It reads the contents of the source file using `FileUtils.readFileToString()`.
4. **Splitting the Source File Contents:**
   - It splits the source file contents into individual lines using `split("\n")`.
5. **Creating a List for Transformed Lines:**
   - An empty list `transformedLines` is created to store the transformed lines.
6. **Iterating Through Lines:**
   - It iterates through each line in the source file using the `each` method.
7. **Processing Each Line:**
   - For each line:
     - It trims leading and trailing whitespace using `StringUtils.trim()`.
     - It checks if the line is empty or contains only whitespace using `StringUtils.isBlank()`. If so, it skips the line.
     - It converts the line to uppercase using `StringUtils.upperCase()`.
     - It adds the transformed line to the `transformedLines` list.
8. **Converting the Transformed Lines to a String:**
   - Once all lines are processed, it converts the list of transformed lines back into a string using `join("\n")`.
9. **Writing the Transformed File Contents:**
   - It writes the transformed file contents to the destination file using `FileUtils.writeStringToFile()`.
10. **Printing a Success Message:**
    - It prints a message to the console indicating that the transformation was successful.

To run this script, you can follow these steps:

1. Save the script as a file with a `.groovy` extension (e.g., `transform-file.groovy`).
2. Open a command-line terminal.
3. Navigate to the directory where the script is saved.
4. Run the script using the following command:

```
groovy transform-file.groovy source-file-path destination-file-path
```

Replace `source-file-path` with the actual path to your source file and `destination-file-path` with the desired path for the transformed file.

This script demonstrates a complex and differentiated code that involves reading and writing files, string manipulation, and conditional logic. It is a good example of how Groovy can be used for text processing tasks.