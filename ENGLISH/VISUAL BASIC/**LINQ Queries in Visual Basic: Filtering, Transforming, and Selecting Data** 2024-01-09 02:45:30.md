```visual basic

Public Class Form1

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim myList As New List(Of String)
        myList.Add("One")
        myList.Add("Two")
        myList.Add("Three")
        myList.Add("Four")
        myList.Add("Five")

        Dim myDictionary As New Dictionary(Of String, Integer)
        myDictionary.Add("One", 1)
        myDictionary.Add("Two", 2)
        myDictionary.Add("Three", 3)
        myDictionary.Add("Four", 4)
        myDictionary.Add("Five", 5)

        Dim myLinqQuery = From item In myList
                          Where item.Length > 3
                          Select item

        For Each item In myLinqQuery
            Console.WriteLine(item)
        Next

        Dim myLambdaQuery = myList.Where(Function(x) x.Length > 3)

        For Each item In myLambdaQuery
            Console.WriteLine(item)
        Next

        Dim myAnonymousTypeQuery = From item In myList
                                  Select New With {
                                      .Value = item,
                                      .Length = item.Length
                                  }

        For Each item In myAnonymousTypeQuery
            Console.WriteLine("{0} ({1})", item.Value, item.Length)
        Next

        Dim myExtensionMethodQuery = myList.Where(Function(x) x.Length > 3).Select(Function(x) x.ToUpper())

        For Each item In myExtensionMethodQuery
            Console.WriteLine(item)
        Next
    End Sub

End Class

```

This code demonstrates various LINQ queries in Visual Basic. It includes:

1. **List and Dictionary Initialization:**
   - A list named `myList` is initialized with five strings.
   - A dictionary named `myDictionary` is initialized with five key-value pairs.

2. **LINQ Query Using `From...Select` Syntax:**
   - A LINQ query is created using the `From...Select` syntax to filter the items in `myList` that have a length greater than 3.
   - The results are printed to the console using a `For Each` loop.

3. **LINQ Query Using Lambda Expression:**
   - A LINQ query is created using a lambda expression to achieve the same result as the previous query.
   - The results are printed to the console using a `For Each` loop.

4. **LINQ Query Using Anonymous Type:**
   - A LINQ query is created to select an anonymous type with two properties: `Value` and `Length`.
   - The results are printed to the console using a `For Each` loop.

5. **LINQ Query Using Extension Methods:**
   - A LINQ query is created using extension methods to combine filtering and transformation operations.
   - The results are printed to the console using a `For Each` loop.

This code provides a comprehensive overview of LINQ queries in Visual Basic, covering different syntax options and query types.