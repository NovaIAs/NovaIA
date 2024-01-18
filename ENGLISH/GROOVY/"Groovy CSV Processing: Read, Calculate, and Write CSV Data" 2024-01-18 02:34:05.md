```groovy
// Read and parse a CSV file, perform some calculations, and write the results to a new CSV file

// Define the input and output CSV files
def inputFile = "input.csv"
def outputFile = "output.csv"

// Read the input CSV file
def csvReader = new groovy.csv.CsvReader(new File(inputFile))

// Create a list to store the parsed data
def data = []

// Read each line of the CSV file
csvReader.eachLine { line ->
    // Split the line into individual fields
    def fields = line.split(",")

    // Parse the fields and perform calculations
    def id = fields[0]
    def name = fields[1]
    def age = fields[2].toInteger()
    def salary = fields[3].toBigDecimal()
    def bonus = salary * 0.1

    // Add the parsed data to the list
    data << [id, name, age, salary, bonus]
}

// Create a CSV writer to write the results
def csvWriter = new groovy.csv.CsvWriter(new File(outputFile))

// Write the header row
csvWriter.writeLine(["ID", "Name", "Age", "Salary", "Bonus"])

// Write the data rows
data.each { row ->
    csvWriter.writeLine(row)
}

// Close the CSV writer
csvWriter.close()

// Print a success message
println("CSV file processing complete!")
```

This Groovy code reads and parses a CSV file, performs some calculations on the data, and writes the results to a new CSV file. Let's explain the code step by step:

1. **Define Input and Output CSV Files:**
   - Define the input CSV file name as `inputFile` and the output CSV file name as `outputFile`.

2. **Read Input CSV File:**
   - Create a CSV reader object `csvReader` to read the input CSV file.

3. **Parse CSV Data:**
   - Use `csvReader.eachLine { line -> ... }` to iterate through each line of the CSV file.
   - For each line, split it into individual fields using `line.split(",")`.
   - Convert the fields to the appropriate data types (e.g., `toInteger()` for integers, `toBigDecimal()` for decimals).
   - Perform any necessary calculations (in this example, calculating a bonus).
   - Add the parsed data to the `data` list.

4. **Create CSV Writer:**
   - Create a CSV writer object `csvWriter` to write to the output CSV file.

5. **Write Header Row:**
   - Write the header row to the output CSV file, which contains column names like "ID", "Name", "Age", "Salary", and "Bonus".

6. **Write Data Rows:**
   - Iterate through the `data` list and write each row of data to the output CSV file.

7. **Close CSV Writer:**
   - Close the CSV writer to save the output CSV file.

8. **Success Message:**
   - Print a success message to indicate that the CSV file processing is complete.

This code performs a specific task of reading and parsing a CSV file, performing calculations, and writing the results to another CSV file. You can modify the code to read a different CSV file, perform different calculations, or write the results in a different format as needed for your specific use case.