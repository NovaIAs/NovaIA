```visual basic
Option Explicit

' Define a custom object type to represent a person.
Type Person
    Name As String
    Age As Integer
    Occupation As String
End Type

' Create an array of Person objects.
Dim people() As Person

' Populate the array with some data.
people(0).Name = "John Smith"
people(0).Age = 30
people(0).Occupation = "Software Engineer"

people(1).Name = "Jane Doe"
people(1).Age = 25
people(1).Occupation = "Accountant"

people(2).Name = "Michael Jones"
people(2).Age = 40
people(2).Occupation = "Doctor"

' Loop through the array and display the information about each person.
For i = 0 To UBound(people)
    Debug.Print people(i).Name & ", " & people(i).Age & ", " & people(i).Occupation
Next i

' Define a function to calculate the average age of the people in the array.
Function AverageAge() As Double
    Dim totalAge As Integer
    For i = 0 To UBound(people)
        totalAge = totalAge + people(i).Age
    Next i
    AverageAge = totalAge / UBound(people)
End Function

' Display the average age of the people in the array.
Debug.Print "Average age: " & AverageAge()

' Define a function to find the oldest person in the array.
Function OldestPerson() As Person
    Dim oldestPerson As Person
    oldestPerson.Age = 0
    For i = 0 To UBound(people)
        If people(i).Age > oldestPerson.Age Then
            oldestPerson = people(i)
        End If
    Next i
    OldestPerson = oldestPerson
End Function

' Display the name of the oldest person in the array.
Debug.Print "Oldest person: " & OldestPerson().Name

' Define a function to find the youngest person in the array.
Function YoungestPerson() As Person
    Dim youngestPerson As Person
    youngestPerson.Age = 1000
    For i = 0 To UBound(people)
        If people(i).Age < youngestPerson.Age Then
            youngestPerson = people(i)
        End If
    Next i
    YoungestPerson = youngestPerson
End Function

' Display the name of the youngest person in the array.
Debug.Print "Youngest person: " & YoungestPerson().Name

```

This code defines a custom object type called Person, which has three properties: Name, Age, and Occupation. It then creates an array of Person objects and populates it with some data. The code then loops through the array and displays the information about each person.

The code also defines three functions: AverageAge(), OldestPerson(), and YoungestPerson(). The AverageAge() function calculates the average age of the people in the array. The OldestPerson() function finds the oldest person in the array. The YoungestPerson() function finds the youngest person in the array.

Finally, the code displays the average age of the people in the array, the name of the oldest person in the array, and the name of the youngest person in the array.

This code is complex and differentiated because it uses a custom object type, an array, and three functions. It also uses a loop to iterate through the array and display the information about each person. This code is not likely to be repeated again because it is very specific to the task of finding the average age, oldest person, and youngest person in an array of Person objects.