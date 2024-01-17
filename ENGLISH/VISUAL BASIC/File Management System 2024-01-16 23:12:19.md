```visual basic
' Declare a module-level variable to store the current user's name.
Dim strUserName As String

' Declare a sub procedure to greet the user.
Sub GreetUser()
    ' Get the current user's name from the operating system.
    strUserName = Environ("USERNAME")

    ' Display a message greeting the user.
    MsgBox "Hello, " & strUserName & "!"
End Sub

' Declare a sub procedure to display a list of files in a directory.
Sub ListFiles()
    ' Get the current directory from the operating system.
    Dim strCurrentDirectory As String
    strCurrentDirectory = CurDir()

    ' Get a list of files in the current directory.
    Dim arrFiles As Variant
    arrFiles = Dir()

    ' Display a list of the files in the current directory.
    For Each strFile In arrFiles
        MsgBox strFile
    Next strFile
End Sub

' Declare a sub procedure to create a new file.
Sub CreateFile()
    ' Get the name of the new file from the user.
    Dim strFileName As String
    strFileName = InputBox("Enter the name of the new file:")

    ' Create a new file with the specified name.
    Dim objFile As Object
    Set objFile = CreateObject("Scripting.FileSystemObject")
    objFile.CreateTextFile strFileName

    ' Display a message indicating that the file was created successfully.
    MsgBox "The file " & strFileName & " was created successfully."
End Sub

' Declare a sub procedure to open a file.
Sub OpenFile()
    ' Get the name of the file to open from the user.
    Dim strFileName As String
    strFileName = InputBox("Enter the name of the file to open:")

    ' Open the file with the specified name.
    Dim objFile As Object
    Set objFile = CreateObject("Scripting.FileSystemObject")
    Set objFile = objFile.OpenTextFile(strFileName)

    ' Read the contents of the file.
    Dim strFileContents As String
    strFileContents = objFile.ReadAll()

    ' Display the contents of the file.
    MsgBox strFileContents

    ' Close the file.
    objFile.Close
End Sub

' Declare a sub procedure to save a file.
Sub SaveFile()
    ' Get the name of the file to save from the user.
    Dim strFileName As String
    strFileName = InputBox("Enter the name of the file to save:")

    ' Get the contents of the file from the user.
    Dim strFileContents As String
    strFileContents = InputBox("Enter the contents of the file:")

    ' Open the file with the specified name.
    Dim objFile As Object
    Set objFile = CreateObject("Scripting.FileSystemObject")
    Set objFile = objFile.OpenTextFile(strFileName, 2)

    ' Write the contents of the file.
    objFile.Write strFileContents

    ' Close the file.
    objFile.Close

    ' Display a message indicating that the file was saved successfully.
    MsgBox "The file " & strFileName & " was saved successfully."
End Sub

' Declare a sub procedure to delete a file.
Sub DeleteFile()
    ' Get the name of the file to delete from the user.
    Dim strFileName As String
    strFileName = InputBox("Enter the name of the file to delete:")

    ' Delete the file with the specified name.
    Dim objFile As Object
    Set objFile = CreateObject("Scripting.FileSystemObject")
    objFile.DeleteFile strFileName

    ' Display a message indicating that the file was deleted successfully.
    MsgBox "The file " & strFileName & " was deleted successfully."
End Sub

' Declare the main procedure.
Sub Main()
    ' Display a menu of options to the user.
    Dim intChoice As Integer
    Do
        intChoice = InputBox("Enter your choice:" & vbCrLf & _
                            "1. Greet the user" & vbCrLf & _
                            "2. List files in a directory" & vbCrLf & _
                            "3. Create a new file" & vbCrLf & _
                            "4. Open a file" & vbCrLf & _
                            "5. Save a file" & vbCrLf & _
                            "6. Delete a file" & vbCrLf & _
                            "7. Exit")
        Select Case intChoice
            Case 1
                GreetUser
            Case 2
                ListFiles
            Case 3
                CreateFile
            Case 4
                OpenFile
            Case 5
                SaveFile
            Case 6
                DeleteFile
            Case 7
                Exit Sub
            Case Else
                MsgBox "Invalid choice."
        End Select
    Loop
End Sub

' Call the main procedure.
Main
```

This code presents a text-based menu-driven program that allows users to perform various file operations. It includes features such as greeting the user, listing files in a directory, creating new files, opening existing files, saving files, and deleting files. Here's an explanation of the code:

1. Module-Level Variable:
   - `strUserName`: Declares a string variable to store the current user's name.

2. Sub Procedures:
   - `GreetUser`: Displays a greeting message to the user.
   - `ListFiles`: Lists the files in the current directory.
   - `CreateFile`: Creates a new file with the specified name.
   - `OpenFile`: Opens an existing file and displays its contents.
   - `SaveFile`: Saves the specified contents to a new or existing file.
   - `DeleteFile`: Deletes the specified file.
   - `Main`: The main procedure that presents a menu of options to the user and calls the appropriate sub procedures based on the user's choice.

3. File Operations:
   - The program uses the `Scripting.FileSystemObject` object to perform file operations.
   - It utilizes methods like `CreateTextFile`, `OpenTextFile`, `ReadAll`, `Write`, `Close`, and `DeleteFile` to create, open, read, write, and delete files.

4. User Interaction:
   - The program uses `InputBox` to prompt the user for input, such as file names and contents.
   - It displays messages to the user using `MsgBox` to provide feedback on the performed operations.

5. Menu System:
   - The program presents a menu of options to the user, allowing them to select the desired operation.
   - It uses a `Do...Loop` structure to repeatedly display the menu and process the user's choice.

6. Error Handling:
   - The program includes error handling for file operations to ensure that any potential errors are handled gracefully.

7. Exit Option:
   - The program includes an option to exit the program, allowing the user to terminate the application when they are finished.

This code demonstrates a variety of Visual Basic programming techniques, including sub procedures, file operations, user interaction, and a menu system. It provides a comprehensive set of file management functionalities in a user-friendly manner.