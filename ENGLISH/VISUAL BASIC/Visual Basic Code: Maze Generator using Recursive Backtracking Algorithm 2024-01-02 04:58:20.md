Certainly! Here's a complex and unique Visual Basic code that generates a random maze using a recursive backtracking algorithm. Let me explain how it works:

```vb
Imports System.Drawing

Public Class MazeGeneratorForm
    Inherits Form

    Private mazeSize As Integer = 20
    Private maze As Boolean(,)
    Private walls As Boolean(,)

    Public Sub New()
        InitializeComponent()
        GenerateMaze()
    End Sub

    Private Sub GenerateMaze()
        maze = New Boolean(mazeSize - 1, mazeSize - 1) {}
        walls = New Boolean(mazeSize * 2 + 1, mazeSize * 2 + 1) {}

        For i As Integer = 0 To mazeSize - 1
            For j As Integer = 0 To mazeSize - 1
                maze(i, j) = True
            Next
        Next

        VisitCell(0, 0)

        DrawMaze()
    End Sub

    Private Sub VisitCell(ByVal x As Integer, ByVal y As Integer)
        maze(x, y) = False

        Dim directions As List(Of Integer) = GetRandomDirections()

        For Each direction As Integer In directions
            Dim nx As Integer = x
            Dim ny As Integer = y

            Select Case direction
                Case 0 ' Up
                    ny -= 1
                Case 1 ' Right
                    nx += 1
                Case 2 ' Down
                    ny += 1
                Case 3 ' Left
                    nx -= 1
            End Select

            If nx >= 0 AndAlso nx < mazeSize AndAlso ny >= 0 AndAlso ny < mazeSize AndAlso maze(nx, ny) Then
                walls(x + nx + 1, y + ny + 1) = True
                VisitCell(nx, ny)
            End If
        Next
    End Sub

    Private Function GetRandomDirections() As List(Of Integer)
        Dim directions As New List(Of Integer) From {0, 1, 2, 3}
        Dim random As New Random()

        For i As Integer = 0 To directions.Count - 1
            Dim temp As Integer = directions(i)
            Dim j As Integer = random.Next(i, directions.Count)
            directions(i) = directions(j)
            directions(j) = temp
        Next

        Return directions
    End Function

    Private Sub DrawMaze()
        Dim mazeImage As New Bitmap(mazeSize * 2 + 1, mazeSize * 2 + 1)
        Dim graphics As Graphics = Graphics.FromImage(mazeImage)
        graphics.Clear(Color.White)

        Dim wallSize As Integer = 10

        For i As Integer = 0 To mazeSize * 2
            For j As Integer = 0 To mazeSize * 2
                If walls(i, j) Then
                    graphics.FillRectangle(Brushes.Black, i * wallSize, j * wallSize, wallSize, wallSize)
                End If
            Next
        Next

        Dim mazeDisplay As New PictureBox()
        mazeDisplay.Image = mazeImage
        mazeDisplay.Size = New Size(mazeSize * wallSize, mazeSize * wallSize)

        Controls.Add(mazeDisplay)
    End Sub

    Private Sub InitializeComponent()
        Me.SuspendLayout()
        Me.ClientSize = New System.Drawing.Size(800, 800)
        Me.Name = "MazeGeneratorForm"
        Me.Text = "Maze Generator"
        Me.ResumeLayout(False)
    End Sub

    Public Shared Sub Main()
        Application.Run(New MazeGeneratorForm())
    End Sub

End Class
```

This code creates a Visual Basic Windows Forms application that generates and displays a random maze. The maze is represented by a two-dimensional boolean array (`maze`) where `True` represents walls and `False` represents paths. The maze generation algorithm used here is the recursive backtracking algorithm.

The `GenerateMaze` method initializes the `maze` and `walls` arrays and starts the maze generation process by calling `VisitCell` for the initial cell (0, 0). The `VisitCell` method visits each cell, randomly selects a direction, and recursively visits the neighboring cells if they haven't been visited yet. The walls between the current cell and the neighboring cell are removed as paths are created.

The `GetRandomDirections` method shuffles the directions (up, right, down, left) randomly so that each direction has an equal chance of being chosen.

The `DrawMaze` method creates a `Bitmap` image and draws the maze walls using black rectangles. The size of each wall is set by the `wallSize` variable. The resulting maze image is then displayed in a `PictureBox` control.

The `MazeGeneratorForm` class is a Windows Forms application that sets up the form and starts the application by calling `Application.Run`.

To use this code, you need to create a new Visual Basic Windows Forms application project, replace the default `Form1` class with the provided code, and run the application. You will see a maze generated and displayed in the form. Feel free to adjust the `mazeSize` variable to change the size of the maze.