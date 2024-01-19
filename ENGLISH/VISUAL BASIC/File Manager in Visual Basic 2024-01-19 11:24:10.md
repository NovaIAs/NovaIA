```visual basic
Public Enum FileTypes
    Text
    Binary
End Enum

Public Class FileReader
    Private m_Path As String
    Private m_Type As FileTypes

    Public Sub New(ByVal path As String, ByVal type As FileTypes)
        m_Path = path
        m_Type = type
    End Sub

    Public Function ReadFile() As String
        Dim stream As Stream
        If m_Type = FileTypes.Text Then
            stream = New StreamReader(m_Path)
        ElseIf m_Type = FileTypes.Binary Then
            stream = New BinaryReader(m_Path)
        End If

        Dim content As String = stream.ReadToEnd()
        stream.Close()
        Return content
    End Function
End Class

Public Class FileWriter
    Private m_Path As String
    Private m_Type As FileTypes

    Public Sub New(ByVal path As String, ByVal type As FileTypes)
        m_Path = path
        m_Type = type
    End Sub

    Public Sub WriteFile(ByVal content As String)
        Dim stream As Stream
        If m_Type = FileTypes.Text Then
            stream = New StreamWriter(m_Path)
        ElseIf m_Type = FileTypes.Binary Then
            stream = New BinaryWriter(m_Path)
        End If

        stream.Write(content)
        stream.Close()
    End Sub
End Class

Public Class FileManager
    Public Function CopyFile(ByVal sourcePath As String, ByVal destinationPath As String)
        Dim reader As FileReader = New FileReader(sourcePath, FileTypes.Binary)
        Dim content As String = reader.ReadFile()

        Dim writer As FileWriter = New FileWriter(destinationPath, FileTypes.Binary)
        writer.WriteFile(content)
    End Function

    Public Function MoveFile(ByVal sourcePath As String, ByVal destinationPath As String)
        If Not IO.File.Exists(sourcePath) Then
            Throw New FileNotFoundException("File not found: " & sourcePath)
        End If

        If IO.File.Exists(destinationPath) Then
            IO.File.Delete(destinationPath)
        End If

        IO.File.Move(sourcePath, destinationPath)
    End Function

    Public Function DeleteFile(ByVal path As String)
        If Not IO.File.Exists(path) Then
            Throw New FileNotFoundException("File not found: " & path)
        End If

        IO.File.Delete(path)
    End Function
End Class

Public Sub Main()
    Dim manager As FileManager = New FileManager()

    manager.CopyFile("c:\source.txt", "c:\destination.txt")
    manager.MoveFile("c:\source.txt", "c:\destination.txt")
    manager.DeleteFile("c:\source.txt")
End Sub
```

This code is a file manager that can copy, move, and delete files. It uses two classes, `FileReader` and `FileWriter`, to read and write files, respectively. The `FileManager` class then uses these classes to perform the file operations.

The `FileReader` and `FileWriter` classes have constructors that take a path and a file type as arguments. The file type can be either `Text` or `Binary`. The `ReadFile()` method of the `FileReader` class reads the entire file and returns the contents as a string. The `WriteFile()` method of the `FileWriter` class writes the contents of a string to a file.

The `FileManager` class has three methods: `CopyFile()`, `MoveFile()`, and `DeleteFile()`. The `CopyFile()` method copies a file from one location to another. The `MoveFile()` method moves a file from one location to another. The `DeleteFile()` method deletes a file.

The `Main()` method of the program creates an instance of the `FileManager` class and calls the `CopyFile()`, `MoveFile()`, and `DeleteFile()` methods to demonstrate the functionality of the class.