```visual basic
'Module-Level Variables
Dim myList As New List(Of Integer)
Dim myDictionary As New Dictionary(Of String, Integer)

'Function to Add Items to a List
Public Sub AddItemsToList()
    myList.Add(1)
    myList.Add(2)
    myList.Add(3)
    myList.Add(4)
    myList.Add(5)
End Sub

'Function to Remove Items from a List
Public Sub RemoveItemsFromList()
    myList.Remove(2)
    myList.Remove(4)
End Sub

'Function to Iterate Through a List
Public Sub IterateThroughList()
    For Each item In myList
        Console.WriteLine(item)
    Next
End Sub

'Function to Add Items to a Dictionary
Public Sub AddItemsToDictionary()
    myDictionary.Add("One", 1)
    myDictionary.Add("Two", 2)
    myDictionary.Add("Three", 3)
    myDictionary.Add("Four", 4)
    myDictionary.Add("Five", 5)
End Sub

'Function to Remove Items from a Dictionary
Public Sub RemoveItemsFromDictionary()
    myDictionary.Remove("Two")
    myDictionary.Remove("Four")
End Sub

'Function to Iterate Through a Dictionary
Public Sub IterateThroughDictionary()
    For Each item In myDictionary
        Console.WriteLine(item.Key & " : " & item.Value)
    Next
End Sub

'Function to Call All Other Functions
Public Sub Main()
    AddItemsToList()
    RemoveItemsFromList()
    IterateThroughList()
    AddItemsToDictionary()
    RemoveItemsFromDictionary()
    IterateThroughDictionary()
End Sub
```

**Explanation:**

1. **Module-Level Variables:**
   - `myList`: A list of integers initialized using the `New` keyword.
   - `myDictionary`: A dictionary of strings mapped to integers, also initialized using `New`.

2. **Function to Add Items to a List (`AddItemsToList`):**
   - Adds five integers (1, 2, 3, 4, 5) to the `myList` using the `Add` method.

3. **Function to Remove Items from a List (`RemoveItemsFromList`):**
   - Removes items with values of 2 and 4 from `myList` using the `Remove` method.

4. **Function to Iterate Through a List (`IterateThroughList`):**
   - Uses a `For Each` loop to iterate through each item in `myList` and prints it to the console.

5. **Function to Add Items to a Dictionary (`AddItemsToDictionary`):**
   - Adds five key-value pairs to the `myDictionary`: "One" mapped to 1, "Two" to 2, "Three" to 3, "Four" to 4, and "Five" to 5.

6. **Function to Remove Items from a Dictionary (`RemoveItemsFromDictionary`):**
   - Removes key-value pairs with keys "Two" and "Four" from `myDictionary` using the `Remove` method.

7. **Function to Iterate Through a Dictionary (`IterateThroughDictionary`):**
   - Uses a `For Each` loop to iterate through each key-value pair in `myDictionary` and prints the key and value to the console.

8. **Function to Call All Other Functions (`Main`):**
   - Calls all other functions in sequence to demonstrate their usage.

This code showcases various data structures (lists and dictionaries), their manipulation (adding and removing items), and iteration through them using loops. It also demonstrates the use of functions to organize and structure the code.