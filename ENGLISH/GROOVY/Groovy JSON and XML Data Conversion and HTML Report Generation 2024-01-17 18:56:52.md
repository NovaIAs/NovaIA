```groovy
import groovy.json.JsonSlurper
import groovy.xml.MarkupBuilder

// Data structure to hold the parsed JSON data
class Person {
    String name
    int age
    String city
}

// Parse the JSON data from a file
def jsonSlurper = new JsonSlurper()
def persons = jsonSlurper.parse(new File("data.json"))

// Convert the JSON data to a list of Person objects
def personList = persons.collect { new Person(name: it.name, age: it.age, city: it.city) }

// Create an XML document to store the Person data
def xmlBuilder = new MarkupBuilder()
xmlBuilder.persons {
    personList.each { person ->
        person(name: person.name, age: person.age, city: person.city)
    }
}

// Save the XML document to a file
def xmlFile = new File("data.xml")
xmlBuilder.writeTo(xmlFile)

// Read the XML document from a file
def xmlSlurper = new XmlSlurper()
def xmlDocument = xmlSlurper.parse(xmlFile)

// Convert the XML data to a list of Person objects
def xmlPersonList = xmlDocument.person.collect { new Person(name: it.@name, age: it.@age.toInteger(), city: it.@city) }

// Print the Person data to the console
println("Person Data:")
xmlPersonList.each { person ->
    println("Name: ${person.name}, Age: ${person.age}, City: ${person.city}")
}
```


```groovy
// Define a class to represent a Student
class Student {
    String name
    int age
    List<String> courses
}

// Define a list of Student objects
def students = [
    new Student(name: 'John Doe', age: 20, courses: ['Math', 'Science', 'History']),
    new Student(name: 'Jane Smith', age: 21, courses: ['English', 'Art', 'Music']),
    new Student(name: 'Michael Jones', age: 22, courses: ['Computer Science', 'Biology', 'Economics'])
]

// Use the Groovy templating engine to generate a report from the student data
def template = """
<!DOCTYPE html>
<html>
<head>
  <title>Student Report</title>
</head>
<body>
  <h1>Student Report</h1>
  <ul>
    ${students.collect { student ->
      "<li>${student.name} - ${student.age} years old - Courses: ${student.courses.join(', ')}</li>"
    }.join('\n')}
  </ul>
</body>
</html>
"""

// Write the report to a file
def reportFile = new File("report.html")
reportFile.write(template)

// Open the report in a web browser
java.awt.Desktop.getDesktop().browse(reportFile.toURI())
```
In this Groovy script, we use the templating engine to generate an HTML report from a list of Student objects. The script first defines a simple Student class to represent student data. It then creates a list of Student objects and uses the Groovy templating engine to generate an HTML report from the student data. The generated report is then written to a file and opened in a web browser using the Java AWT Desktop API.