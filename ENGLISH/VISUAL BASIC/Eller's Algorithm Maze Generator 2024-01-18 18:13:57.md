```visual basic
' This code generates a random maze using the Eller's algorithm.

' Define the dimensions of the maze.
Const WIDTH = 100
Const HEIGHT = 100

' Create a 2D array to represent the maze.
Dim maze() As Integer

' Initialize the maze with walls.
For y = 0 To HEIGHT - 1
    For x = 0 To WIDTH - 1
        maze(x, y) = 1
    Next x
Next y

' Create a stack to store the current cell and its neighbors.
Dim stack() As Long

' Start with the top-left cell.
stack(0) = 0

' While the stack is not empty,
Do While stack(UBound(stack)) <> 0

    ' Pop the current cell from the stack.
    Dim currentCell As Long
    currentCell = stack(UBound(stack))
    stack(UBound(stack)) = 0

    ' Get the current cell's coordinates.
    Dim x As Integer
    Dim y As Integer
    x = currentCell Mod WIDTH
    y = currentCell \ WIDTH

    ' Get the current cell's neighbors.
    Dim neighbors() As Long

    ' Check the cell to the right.
    If x < WIDTH - 1 Then
        neighbors(0) = currentCell + 1
    Else
        neighbors(0) = 0
    End If

    ' Check the cell below.
    If y < HEIGHT - 1 Then
        neighbors(1) = currentCell + WIDTH
    Else
        neighbors(1) = 0
    End If

    ' Check the cell to the left.
    If x > 0 Then
        neighbors(2) = currentCell - 1
    Else
        neighbors(2) = 0
    End If

    ' Check the cell above.
    If y > 0 Then
        neighbors(3) = currentCell - WIDTH
    Else
        neighbors(3) = 0
    End If

    ' Choose a random neighbor.
    Dim randomNeighbor As Long
    randomNeighbor = neighbors(Int(Rnd * 4))

    ' If the random neighbor is not a wall,
    If randomNeighbor <> 0 And maze(randomNeighbor Mod WIDTH, randomNeighbor \ WIDTH) = 1 Then

        ' Knock down the wall between the current cell and the random neighbor.
        maze(x, y) = 0
        maze(randomNeighbor Mod WIDTH, randomNeighbor \ WIDTH) = 0

        ' Push the random neighbor onto the stack.
        stack(UBound(stack) + 1) = randomNeighbor

    End If

' Pop the current cell from the stack.
stack(UBound(stack)) = 0

' If the stack is not empty,
If stack(UBound(stack)) <> 0 Then

    ' Get the current cell's coordinates.
    x = stack(UBound(stack)) Mod WIDTH
    y = stack(UBound(stack)) \ WIDTH

    ' Get the current cell's neighbors.
    neighbors(0) = 0
    neighbors(1) = 0
    neighbors(2) = 0
    neighbors(3) = 0
    neighbors(4) = 0
    neighbors(5) = 0
    neighbors(6) = 0
    neighbors(7) = 0

    ' Check the cell to the right.
    If x < WIDTH - 1 Then
        neighbors(0) = stack(UBound(stack) - 1) + 1
        neighbors(1) = neighbors(0) + WIDTH
    End If

    ' Check the cell below.
    If y < HEIGHT - 1 Then
        neighbors(2) = stack(UBound(stack) - 1) + WIDTH
        neighbors(3) = neighbors(2) + 1
    End If

    ' Check the cell to the left.
    If x > 0 Then
        neighbors(4) = stack(UBound(stack) - 1) - 1
        neighbors(5) = neighbors(4) + WIDTH
    End If

    ' Check the cell above.
    If y > 0 Then
        neighbors(6) = stack(UBound(stack) - 1) - WIDTH
        neighbors(7) = neighbors(6) - 1
    End If

    ' Count the number of unvisited neighbors.
    Dim unvisitedNeighbors As Integer
    unvisitedNeighbors = 0
    For i = 0 To 7
        If neighbors(i) <> 0 And maze(neighbors(i) Mod WIDTH, neighbors(i) \ WIDTH) = 1 Then
            unvisitedNeighbors = unvisitedNeighbors + 1
        End If
    Next i

    ' If there is only one unvisited neighbor,
    If unvisitedNeighbors = 1 Then

        ' Push the unvisited neighbor onto the stack.
        For i = 0 To 7
            If neighbors(i) <> 0 And maze(neighbors(i) Mod WIDTH, neighbors(i) \ WIDTH) = 1 Then
                stack(UBound(stack) + 1) = neighbors(i)
                Exit For
            End If
        Next i

    End If

End If

Loop

' Display the maze.
For y = 0 To HEIGHT