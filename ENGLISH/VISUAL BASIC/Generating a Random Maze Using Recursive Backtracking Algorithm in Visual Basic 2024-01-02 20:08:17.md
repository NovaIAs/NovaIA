Certainly! Below is a complex and unique code written in Visual Basic that generates a random maze using the recursive backtracking algorithm. This algorithm creates a maze by randomly carving passages through a grid, ensuring that every cell is visited only once.

```vb
Imports System

Public Class MazeGenerator
    Private maze As Integer(,)
    Private width As Integer
    Private height As Integer
    Private random As Random

    Public Sub New(ByVal width As Integer, ByVal height As Integer)
        Me.width = width
        Me.height = height
        maze = New Integer(height - 1, width - 1) {}
        random = New Random()
    End Sub

    Public Function GenerateMaze() As Integer(,)
        InitializeMaze()
        CarvePassages(0, 0)
        Return maze
    End Function

    Private Sub InitializeMaze()
        For i As Integer = 0 To height - 1
            For j As Integer = 0 To width - 1
                maze(i, j) = 1
            Next
        Next
    End Sub

    Private Sub CarvePassages(ByVal row As Integer, ByVal col As Integer)
        Dim directions() As Integer = {0, 1, 2, 3}
        ShuffleArray(directions)

        For Each direction As Integer In directions
            Dim newRow As Integer = row
            Dim newCol As Integer = col

            Select Case direction
                Case 0 ' Up
                    newRow -= 2
                Case 1 ' Right
                    newCol += 2
                Case 2 ' Down
                    newRow += 2
                Case 3 ' Left
                    newCol -= 2
            End Select

            If IsValidCell(newRow, newCol) Then
                If maze(newRow, newCol) = 1 Then
                    maze(newRow, newCol) = 0 ' Carved passage
                    maze(row + (newRow - row) \ 2, col + (newCol - col) \ 2) = 0 ' Carved passage
                    CarvePassages(newRow, newCol)
                End If
            End If
        Next
    End Sub

    Private Function IsValidCell(ByVal row As Integer, ByVal col As Integer) As Boolean
        Return row >= 0 AndAlso row < height AndAlso col >= 0 AndAlso col < width
    End Function

    Private Sub ShuffleArray(ByVal array() As Integer)
        For i As Integer = array.Length - 1 To 0 Step -1
            Dim j As Integer = random.Next(i + 1)
            Dim temp As Integer = array(i)
            array(i) = array(j)
            array(j) = temp
        Next
    End Sub

    Public Sub PrintMaze()
        For i As Integer = 0 To height - 1
            For j As Integer = 0 To width - 1
                Console.Write(If(maze(i, j) = 0, "  ", "██"))
            Next
            Console.WriteLine()
        Next
    End Sub
End Class

Public Module MazeGeneratorApp
    Public Sub Main()
        Dim width As Integer = 21 ' Must be an odd number
        Dim height As Integer = 21 ' Must be an odd number

        Dim generator As New MazeGenerator(width, height)
        Dim maze As Integer(,) = generator.GenerateMaze()
        generator.PrintMaze()
    End Sub
End Module
```

Explanation:

- The code defines a `MazeGenerator` class and a `MazeGeneratorApp` module.
- The `MazeGenerator` class is responsible for generating the maze using the recursive backtracking algorithm.
- The `MazeGenerator` class has a constructor that takes the width and height of the maze as parameters.
- The `GenerateMaze` method initializes the maze and starts carving passages from the top-left corner.
- The `CarvePassages` method recursively carves passages in random directions until all cells have been visited.
- The `IsValidCell` method checks if a given cell is within the maze boundaries.
- The `ShuffleArray` method shuffles the directions array to randomize the carving order.
- The `PrintMaze` method prints the maze. A `0` represents a carved passage, and `██` represents a wall.
- The `MazeGeneratorApp` module defines the `Main` method which creates a `MazeGenerator` instance and prints the generated maze.

To execute this code, you need to run the `MazeGeneratorApp.Main` method. The `width` and `height` variables can be adjusted to change the size of the maze. The generated maze will be displayed in the console.