```visual basic

' Define a custom class to represent a person.
Public Class Person
    ' Declare private fields to store the person's name and age.
    Private _name As String
    Private _age As Integer

    ' Define a public property to get and set the person's name.
    Public Property Name() As String
        Get
            Return _name
        End Get
        Set(value As String)
            _name = value
        End Set
    End Property

    ' Define a public property to get and set the person's age.
    Public Property Age() As Integer
        Get
            Return _age
        End Get
        Set(value As Integer)
            _age = value
        End Set
    End Property

    ' Define a constructor to initialize the person's name and age.
    Public Sub New(name As String, age As Integer)
        _name = name
        _age = age
    End Sub

    ' Define a method to return the person's name and age as a string.
    Public Function ToString() As String
        Return String.Format("{0} is {1} years old.", _name, _age)
    End Function
End Class

' Define a custom class to represent a list of people.
Public Class PeopleList
    ' Declare a private field to store the list of people.
    Private _people As List(Of Person)

    ' Define a public property to get the number of people in the list.
    Public ReadOnly Property Count() As Integer
        Get
            Return _people.Count
        End Get
    End Property

    ' Define a public method to add a person to the list.
    Public Sub Add(person As Person)
        _people.Add(person)
    End Sub

    ' Define a public method to remove a person from the list.
    Public Sub Remove(person As Person)
        _people.Remove(person)
    End Sub

    ' Define a public method to get a person from the list by their index.
    Public Function GetAt(index As Integer) As Person
        Return _people(index)
    End Function

    ' Define a public method to iterate over the people in the list.
    Public Function GetEnumerator() As IEnumerator(Of Person)
        Return _people.GetEnumerator()
    End Function

    ' Define a method to return the people in the list as a string.
    Public Function ToString() As String
        Dim sb As New StringBuilder()
        For Each person In _people
            sb.AppendLine(person.ToString())
        Next

        Return sb.ToString()
    End Function
End Class

' Create a new instance of the PeopleList class.
Dim people As New PeopleList()

' Add some people to the list.
people.Add(New Person("John Doe", 25))
people.Add(New Person("Jane Doe", 30))
people.Add(New Person("John Smith", 35))

' Print the people in the list to the console.
Console.WriteLine(people.ToString())

' Remove a person from the list.
people.Remove(people.GetAt(1))

' Print the people in the list to the console again.
Console.WriteLine(people.ToString())

' Iterate over the people in the list using a foreach loop.
For Each person In people
    Console.WriteLine(person.ToString())
Next

```

This code creates a custom class called `Person` that represents a person with a name and age. It also creates a custom class called `PeopleList` that represents a list of people. The `PeopleList` class has methods to add, remove, and get people from the list, as well as a method to iterate over the people in the list.

The code then creates a new instance of the `PeopleList` class and adds some people to the list. It then prints the people in the list to the console. Next, the code removes a person from the list and prints the people in the list to the console again. Finally, the code iterates over the people in the list using a foreach loop and prints each person's name and age to the console.