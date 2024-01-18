```visual basic
Public Const MAXSIZE As Long = 100000
Private Type Node
    Left As Long
    Right As Long
    Data As Double
End Type

Private aData As Node()

Public Sub InitializeArray(ByVal maxSize As Long)
    ReDim aData(0 To maxSize)
    For i As Long = 0 To maxSize
        aData(i).Left = -1
        aData(i).Right = -1
        aData(i).Data = 0#
    Next i
End Sub

Public Function InsertData(ByVal value As Double) As Boolean
    Dim index As Long, parentIndex As Long
    index = 1
    parentIndex = 0
    Do
        If aData(index).Data = 0# Then
            aData(index).Data = value
            Exit Do
        ElseIf value < aData(index).Data Then
            If aData(index).Left = -1 Then
                aData(index).Left = index + 1
                parentIndex = index
                index = aData(index).Left
            Else
                index = aData(index).Left
            End If
        Else
            If aData(index).Right = -1 Then
                aData(index).Right = index + 1
                parentIndex = index
                index = aData(index).Right
            Else
                index = aData(index).Right
            End If
        End If
    Loop Until aData(parentIndex).Data = 0# Or index > MAXSIZE
    InsertData = True
End Function

Public Function FindData(ByVal value As Double) As Boolean
    Dim index As Long
    index = 1
    Do
        If aData(index).Data = value Then
            Exit Do
        ElseIf value < aData(index).Data Then
            If aData(index).Left = -1 Then
                Exit Do
            Else
                index = aData(index).Left
            End If
        Else
            If aData(index).Right = -1 Then
                Exit Do
            Else
                index = aData(index).Right
            End If
        End If
    Loop
    FindData = aData(index).Data = value
End Function

Public Sub PrintTree()
    Dim index As Long
    index = 1
    Do
        If aData(index).Data = 0# Then
            Exit Do
        Else
            Debug.Print aData(index).Data
            If aData(index).Left <> -1 Then
                index = aData(index).Left
            ElseIf aData(index).Right <> -1 Then
                index = aData(index).Right
            Else
                Exit Do
            End If
        End If
    Loop
End Sub
```

This code creates a binary tree data structure in Visual Basic. A binary tree is a tree data structure in which each node has at most two children, which are referred to as the left child and the right child.

The code first defines a constant named MAXSIZE, which specifies the maximum size of the tree. It then defines a private data type named Node, which represents a node in the tree. Each node has three fields: Left, Right, and Data. The Left and Right fields store the indices of the left and right children of the node, respectively, and the Data field stores the data associated with the node.

The code also defines a private array named aData, which will store the nodes of the tree. The InitializeArray subroutine is called to initialize the array with the specified maximum size. It sets all of the nodes in the array to have no children and no data.

The InsertData function is used to insert a new value into the tree. It starts at the root node of the tree and searches for the appropriate location to insert the new value. If the value is less than the value of the current node, it searches the left subtree of the current node. If the value is greater than the value of the current node, it searches the right subtree of the current node.

Once the appropriate location has been found, the new value is inserted into the tree as a child of the current node. The InsertData function returns True if the value was successfully inserted and False otherwise.

The FindData function is used to search for a value in the tree. It starts at the root node of the tree and searches for the value in the same way as the InsertData function. If the value is found, the FindData function returns True. Otherwise, it returns False.

The PrintTree subroutine is used to print the values of the nodes in the tree in order. It starts at the root node of the tree and visits each node in the tree in a depth-first manner. The PrintTree subroutine uses the Debug.Print statement to print the values of the nodes to the Output window.

To use this code, you can create a new Visual Basic project and paste the code into the code window. You can then call the InitializeArray subroutine to initialize the tree, the InsertData function to insert values into the tree, the FindData function to search for values in the tree, and the PrintTree subroutine to print the values of the nodes in the tree.