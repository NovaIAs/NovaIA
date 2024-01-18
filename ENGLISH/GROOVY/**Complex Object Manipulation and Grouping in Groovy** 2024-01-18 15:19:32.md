```groovy
// Define a class to represent a complex object with multiple fields
class ComplexObject {
    String name
    List<String> values
    Map<String, Integer> counts
    Date createdDate
}

// Create a list of complex objects
List<ComplexObject> complexObjects = []

// Populate the list with random data
for (i in 0..<100) {
    ComplexObject object = new ComplexObject()
    object.name = "Object ${i}"
    object.values = ["Value ${j}" for (j in 0..<10)]
    object.counts = ["Count ${k}": (int) (Math.random() * 100) for (k in 0..<5)]
    object.createdDate = new Date()
    complexObjects.add(object)
}

// Filter the list of complex objects based on a condition
List<ComplexObject> filteredObjects = complexObjects.findAll { it.createdDate > new Date() - 24 * 60 * 60 * 1000 }

// Group the filtered objects by their 'name' field
Map<String, List<ComplexObject>> groupedObjects = filteredObjects.groupBy { it.name }

// Print the results for each group
groupedObjects.each { name, objects ->
    println "Group: ${name}"
    objects.each { object ->
        println "\t${object.name}"
        println "\t${object.values}"
        println "\t${object.counts}"
        println "\t${object.createdDate}"
    }
}
```

Explanation:

1. We define a class `ComplexObject` with various fields: `name`, `values`, `counts`, and `createdDate`. This class represents the complex objects we want to work with.

2. We create a list `complexObjects` and populate it with 100 randomly generated `ComplexObject` instances.

3. We filter the list `complexObjects` to include only objects created within the last 24 hours.

4. We group the filtered objects by their `name` field using the `groupBy` method, resulting in a `Map` where each key is a unique `name` and the value is a `List` of `ComplexObject` instances with the same name.

5. Finally, we iterate over each group and print the details of the `ComplexObject` instances within that group, including their `name`, `values`, `counts`, and `createdDate`.

This code demonstrates a complex Groovy script that combines data manipulation, filtering, grouping, and printing operations on a list of `ComplexObject` instances. It showcases various Groovy features such as anonymous classes, list comprehension, method chaining, and closure-based constructs.