// This Groovy script creates a dynamic HTML table based on data from a CSV file.
// The data in the CSV file is assumed to have the following format:
// - Each line represents a row in the table.
// - Each field in a line is separated by a comma.
// - The first line contains the column headers.

// Import the necessary libraries.
import groovy.json.JsonSlurper

// Define the path to the CSV file.
def csvFile = 'data.csv'

// Read the CSV file into a list of lines.
def lines = new File(csvFile).readLines()

// Create a JSON object from the CSV data.
def json = new JsonSlurper().parseText('{' + lines.join(',') + '}')

// Extract the column headers from the JSON object.
def headers = json.keySet().toList()

// Create a new HTML table.
def table = new StringBuilder()
table.append("""
<table border="1">
  <tr>
""")

// Add the column headers to the table.
for (header in headers) {
  table.append("    <th>${header}</th>")
}

// Add the data rows to the table.
table.append("  </tr>")
for (row in json) {
  table.append("  <tr>")
  for (header in headers) {
    table.append("    <td>${row[header]}</td>")
  }
  table.append("  </tr>")
}

// Close the table.
table.append("""
  </tr>
</table>
""")

// Print the HTML table to the console.
println table