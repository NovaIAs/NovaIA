```groovy
// Importing Necessary Libraries
import groovy.json.JsonSlurper
import groovy.xml.MarkupBuilder

// JSON Data
def json = new JsonSlurper().parseText("""
{
  "customers": [
    {
      "id": 1,
      "name": "John Doe",
      "address": "123 Main Street",
      "city": "Anytown",
      "state": "CA",
      "zip": "12345"
    },
    {
      "id": 2,
      "name": "Jane Smith",
      "address": "456 Elm Street",
      "city": "Anytown",
      "state": "CA",
      "zip": "12345"
    }
  ]
}
""")

// XML Generation
def xml = new MarkupBuilder()
xml.customers {
  json.customers.each { customer ->
    customer {
      id(customer.id)
      name(customer.name)
      address(customer.address)
      city(customer.city)
      state(customer.state)
      zip(customer.zip)
    }
  }
}

// Print the XML
println xml

def html = new MarkupBuilder()
html.html {
  head {
    title("Customers")
  }
  body {
    h1("Customers")
    ul {
      json.customers.each { customer ->
        li {
          a(href: "#customer-${customer.id}", customer.name)
        }
      }
    }
    div(id: "customers") {
      json.customers.each { customer ->
        div(id: "customer-${customer.id}", class: "customer") {
          h2(customer.name)
          p(customer.address)
          p("${customer.city}, ${customer.state} ${customer.zip}")
        }
      }
    }
  }
}

// Print the HTML
println html

```

**Explanation:**

This Groovy script performs the following tasks:

1. **JSON Parsing:** It uses the `JsonSlurper` class to parse a JSON string containing an array of customer objects.

2. **XML Generation:** It then uses the `MarkupBuilder` class to generate an XML document representing the customer data. The XML is structured with a `<customers>` root element containing individual `<customer>` elements for each customer.

3. **HTML Generation:** Finally, it uses the `MarkupBuilder` class again to generate an HTML document displaying the customer data. The HTML includes a bulleted list of customer names with links, and a section containing the details of each customer.

4. **Output:** The script prints the generated XML and HTML documents to the console.

This script showcases Groovy's ability to work with JSON and XML data, generate HTML dynamically, and produce formatted output.