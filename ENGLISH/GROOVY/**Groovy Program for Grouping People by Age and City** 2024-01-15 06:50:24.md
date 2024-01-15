```groovy
// Define a Groovy closure to handle the processing of each line
def processLine = { line ->
    // Split the line into its components using a comma as the delimiter
    def parts = line.split(",")

    // Extract the individual fields from the parts array
    def name = parts[0]
    def age = Integer.parseInt(parts[1])
    def city = parts[2]

    // Create a map to store the extracted fields
    def person = [:]
    person["name"] = name
    person["age"] = age
    person["city"] = city

    // Add the person map to a list for further processing
    people << person
}

// Read the input file line by line and process each line using the processLine closure
new File("input.txt").eachLine(processLine)

// Group the people by their age using the groupBy() method
def peopleByAge = people.groupBy { it.age }

// Iterate over the groups and print the average age and the names of the people in each group
peopleByAge.each { age, people ->
    def averageAge = people.collect { it.age }.sum() / people.size()
    println "Average age for group ${age} is ${averageAge}"
    println "People in group ${age} are:"
    people.each { person ->
        println "\t${person.name}"
    }
}

// Group the people by their city using the groupBy() method
def peopleByCity = people.groupBy { it.city }

// Iterate over the groups and print the city and the names of the people in each group
peopleByCity.each { city, people ->
    println "People in city ${city} are:"
    people.each { person ->
        println "\t${person.name}"
    }
}
```

Explanation:

1. We define a Groovy closure called `processLine` that takes a line of text as input and extracts the name, age, and city fields from it. The extracted fields are stored in a map, which is then added to a list called `people`.

2. We read the input file line by line using the `eachLine()` method and pass each line to the `processLine` closure for processing.

3. We use the `groupBy()` method to group the people by their age and store the result in a map called `peopleByAge`.

4. We iterate over the groups in `peopleByAge` and calculate the average age and the names of the people in each group.

5. We use the `groupBy()` method again to group the people by their city and store the result in a map called `peopleByCity`.

6. We iterate over the groups in `peopleByCity` and print the city and the names of the people in each group.