```visual basic
' Define a custom class to represent a person
Public Class Person
    Private _name As String  ' The person's name
    Private _age As Integer  ' The person's age

    ' Constructor to initialize a new person object
    Public Sub New(name As String, age As Integer)
        _name = name
        _age = age
    End Sub

    ' Property to get the person's name
    Public ReadOnly Property Name As String
        Get
            Return _name
        End Get
    End Property

    ' Property to get the person's age
    Public ReadOnly Property Age As Integer
        Get
            Return _age
        End Get
    End Property

    ' Method to greet the person
    Public Sub Greet()
        Console.WriteLine("Hello, my name is {0} and I am {1} years old.", _name, _age)
    End Sub
End Class

' Create an array of Person objects
Dim people As Person() = {
    New Person("John", 25),
    New Person("Mary", 30),
    New Person("Bob", 35)
}

' Iterate over the array and greet each person
For Each person In people
    person.Greet()
Next

' Keep the console window open so the user can see the output
Console.ReadKey()
```

**Explanation:**

* The code defines a custom class named `Person` that represents a person with a name and an age.
* The class has a constructor to initialize a new person object with a name and an age.
* It also has two read-only properties, `Name` and `Age`, that return the person's name and age, respectively.
* The class also has a method named `Greet()` that greets the person by displaying their name and age to the console.
* The code creates an array of `Person` objects and initializes it with three people: John (25 years old), Mary (30 years old), and Bob (35 years old).
* The code then iterates over the array and calls the `Greet()` method on each person to greet them.
* Finally, the code keeps the console window open so the user can see the output.