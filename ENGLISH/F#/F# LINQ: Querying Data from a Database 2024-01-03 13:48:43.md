```f#
// Import the necessary libraries.
open System
open System.Linq
open Microsoft.FSharp.Data.SqlClient
open Microsoft.FSharp.Data.SqlClient.Types

// Define a connection string to the database.
let connectionString =
    @"Server=localhost;
    Database=AdventureWorks;
    Trusted_Connection=True;"

// Create a new data context.
let db = new AdventureWorksDataContext(connectionString)

// Get all the products in the database.
let products =
    db.Products
    |> Seq.filter (fun p -> p.ProductID > 0)
    |> Seq.groupBy (fun p -> p.ProductCategoryID)
    |> Seq.sortBy (fun g -> g.Key)

// Print the product categories and products.
for category in products do
    printfn "%s" category.Key.ToString()
    for product in category do
        printfn "\t%s" product.Name
```

This code demonstrates the following:

* **LINQ support:** F# supports LINQ, which allows you to query data in a declarative way. The `Seq.filter` and `Seq.groupBy` functions are used to filter and group the products by their category.
* **Pattern matching:** F# supports pattern matching, which allows you to match values against a pattern. The `match` expression is used to match the `g` variable against a pattern that specifies the key of the grouping.
* **Immutability:** F# is an immutable language, which means that variables cannot be changed once they are assigned. This makes it easier to reason about your code and avoid bugs.
* **Functional programming:** F# is a functional programming language, which means that it emphasizes the use of functions and values. The `printfn` function is used to print the product categories and products.
* **Type inference:** F# supports type inference, which means that the compiler can automatically infer the type of a variable based on its usage. This makes it easier to write concise code.