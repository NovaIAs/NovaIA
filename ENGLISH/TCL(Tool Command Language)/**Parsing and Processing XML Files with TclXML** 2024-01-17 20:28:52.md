```tcl
# Parsing and Processing XML Files with TclXML
package require Tcl 8.5
package require TclXML

# Define an XML document as a string
set xml "<employees>
  <employee id='1'>
    <name>John Doe</name>
    <age>30</age>
    <salary>1000</salary>
  </employee>
  <employee id='2'>
    <name>Jane Smith</name>
    <age>25</age>
    <salary>1200</salary>
  </employee>
</employees>"

# Parse the XML document into a DOM tree
set doc [xml.dom parse $xml]

# Get the root element of the DOM tree
set root [xml.dom element $doc]

# Iterate over the employee elements in the DOM tree
foreach employee [xml.dom children $root] {
  # Get the employee's ID, name, age, and salary
  set id [xml.dom get $employee "@id"]
  set name [xml.dom get $employee "name"]
  set age [xml.dom get $employee "age"]
  set salary [xml.dom get $employee "salary"]

  # Print the employee's information
  puts "Employee $id:"
  puts "  Name: $name"
  puts "  Age: $age"
  puts "  Salary: $salary"
}
```
**Explanation:**

1. We start by loading the required Tcl and TclXML packages.

2. We define an XML document as a string. This XML document contains information about two employees, including their ID, name, age, and salary.

3. We use `xml.dom parse` to parse the XML document into a DOM tree. The DOM tree is a hierarchical representation of the XML document, where each node represents an element or text content.

4. We get the root element of the DOM tree using `xml.dom element`. The root element is the outermost element in the XML document.

5. We iterate over the employee elements in the DOM tree using `foreach`. For each employee element, we get its ID, name, age, and salary using `xml.dom get`.

6. Finally, we print the employee's information using `puts`.

This code demonstrates how to parse and process XML files in Tcl using the TclXML package. It shows how to load an XML document, parse it into a DOM tree, and extract specific information from the XML elements.