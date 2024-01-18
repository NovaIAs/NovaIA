```visual basic
' This code defines a custom UserControl that displays a scrollable list of items.

' The control has two properties: Items and SelectedItem.
' Items is a collection of strings that are displayed in the list.
' SelectedItem is the currently selected item in the list.

' The control also has two events: ItemClick and SelectedIndexChanged.
' ItemClick is raised when an item in the list is clicked.
' SelectedIndexChanged is raised when the currently selected item in the list changes.

Public Class ScrollableList
    Private _items As New Collection
    Private _selectedItem As String

    ' The Items property returns the collection of items in the list.
    Public Property Items() As Collection
        Get
            Return _items
        End Get
    End Property

    ' The SelectedItem property returns the currently selected item in the list.
    Public Property SelectedItem() As String
        Get
            Return _selectedItem
        End Get
        Set(ByVal value As String)
            _selectedItem = value
            RaiseEvent SelectedIndexChanged(Me, New EventArgs)
        End Set
    End Property

    ' The ItemClick event is raised when an item in the list is clicked.
    Public Event ItemClick As EventHandler(ByVal sender As Object, ByVal e As EventArgs)

    ' The SelectedIndexChanged event is raised when the currently selected item in the list changes.
    Public Event SelectedIndexChanged As EventHandler(ByVal sender As Object, ByVal e As EventArgs)

    ' The constructor creates a new instance of the ScrollableList control.
    Public Sub New()
        ' Initialize the control.
        InitializeComponent()

        ' Set the default values for the Items and SelectedItem properties.
        _items.Add("Item 1")
        _items.Add("Item 2")
        _items.Add("Item 3")
        _selectedItem = _items(0)
    End Sub

    ' The OnPaint method draws the control.
    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        ' Get the graphics object for the control.
        Dim g As Graphics = e.Graphics

        ' Draw the background of the control.
        g.FillRectangle(Brushes.White, e.ClipRectangle)

        ' Draw the items in the list.
        Dim i As Integer = 0
        For Each item In _items
            ' Get the rectangle for the item.
            Dim rect As Rectangle = New Rectangle(0, i * 20, e.ClipRectangle.Width, 20)

            ' Draw the item.
            If item = _selectedItem Then
                g.FillRectangle(Brushes.LightBlue, rect)
            Else
                g.FillRectangle(Brushes.White, rect)
            End If
            g.DrawString(item, Me.Font, Brushes.Black, rect)

            ' Increment the y-coordinate for the next item.
            i += 1
        Next

        ' Draw the border of the control.
        g.DrawRectangle(Pens.Black, e.ClipRectangle)
    End Sub

    ' The OnMouseDown method handles mouse down events.
    Protected Overrides Sub OnMouseDown(ByVal e As MouseEventArgs)
        ' Get the index of the item that was clicked.
        Dim index As Integer = e.Y / 20

        ' If an item was clicked, select it and raise the ItemClick event.
        If index >= 0 And index < _items.Count Then
            _selectedItem = _items(index)
            RaiseEvent ItemClick(Me, New EventArgs)
        End If
    End Sub
End Class
```

This code defines a custom UserControl that displays a scrollable list of items. The control has two properties: Items and SelectedItem. Items is a collection of strings that are displayed in the list. SelectedItem is the currently selected item in the list.

The control also has two events: ItemClick and SelectedIndexChanged. ItemClick is raised when an item in the list is clicked. SelectedIndexChanged is raised when the currently selected item in the list changes.

The constructor creates a new instance of the ScrollableList control. The OnPaint method draws the control. The OnMouseDown method handles mouse down events.