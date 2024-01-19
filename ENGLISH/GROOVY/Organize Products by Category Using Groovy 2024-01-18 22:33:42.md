```groovy
import groovy.json.JsonSlurper

// Define a method to fetch data from a URL
def fetchJson(url) {
    def json = new JsonSlurper().parseText(new URL(url).text)
}

// Define a variable to store the base URL for the API
def baseUrl = "https://example.com/api"

// Fetch a list of all products from the API
def products = fetchJson("${baseUrl}/products")

// Create a map to store product categories and their associated products
def productCategories = [:]

// Iterate over the list of products
products.each { product ->
    // Get the name of the product category
    def category = product.category

    // Add the product to the map of product categories
    if (!productCategories.containsKey(category)) {
        productCategories[category] = []
    }
    productCategories[category] << product.name
}

// Print the map of product categories and their associated products
productCategories.each { category, products ->
    println("${category}: ${products.join(', ')}")
}
```

This Groovy code performs the following tasks:

1. Defines a method called `fetchJson()`, which fetches data from a URL and returns the JSON data as a Groovy object.

2. Configures the base URL for the API as `https://example.com/api`.

3. Fetches a list of all products from the API and stores the result in the `products` variable.

4. Initializes an empty map called `productCategories` to store product categories and their associated products.

5. Iterates over the list of products, retrieves the category of each product, and adds the product to the appropriate category in the `productCategories` map.

6. Prints the map of product categories and their associated products, displaying each category followed by a comma-separated list of its products.

This code demonstrates a more complex and comprehensive use of Groovy for working with JSON data and organizing it into a structured format. It effectively categorizes the products and presents them in a meaningful way.