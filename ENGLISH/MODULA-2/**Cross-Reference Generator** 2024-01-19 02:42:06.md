```modula-2
MODULE CrossRef;
FROM Terminal IMPORT GetChar,;
                     Cls, Eoln, NewLine,;
                     Print, PrintNum, PrintString, PrintStringNL;
FROM FileSys IMPORT AbsName,;
                   Close, Create, Open, Read, Write;
FROM Target IMPORT Execute, TimeStamp;
FROM Strings IMPORT Copy, EqString, Length, SubString;
FROM Vectors IMPORT Append, Capacity, Create, DestructiveDelete,;
                   DestructiveInsert, DestructiveReplace,;
                   Empty, INDEX, Index, Insert, Size;

TYPE Name = ARRAY[0..255] OF CHAR;
TYPE File = ARRAY[0..1023] OF CHAR;
TYPE Ref = RECORD
           File: Name;
           Line: CARDINAL;
           END;
TYPE Cross = RECORD
           Label: Name;
           Refs: ARRAY OF Ref;
           RefCount: CARDINAL;
           END;

VAR Files: Vectors.Vector;
VAR CrossRefs: Vectors.Vector;

PROCEDURE Init;
BEGIN
    Files := Vectors.Create[Name](10);
    CrossRefs := Vectors.Create[Cross](10);
END Init;

PROCEDURE Free;
VAR I: CARDINAL;
BEGIN
    FOR I := Vectors.Capacity(Files) - 1 TO 0 BY -1 DO
        IF NOT Empty(Files) THEN
            Vectors.DestructiveDelete(Files, I)
        END
    END;
    FOR I := Vectors.Capacity(CrossRefs) - 1 TO 0 BY -1 DO
        IF NOT Empty(CrossRefs) THEN
            Vectors.DestructiveDelete(CrossRefs, I)
        END
    END
END Free;

PROCEDURE AddFile(Name: Name);
VAR F, I: CARDINAL;
BEGIN
    F := Vectors.Size(Files);
    Vectors.Insert(Files, F, Name);
    I := Vectors.Size(CrossRefs);
    Vectors.Insert(CrossRefs, I, Cross{Label => Name,
                                     Refs => [],
                                     RefCount => 0})
END AddFile;

PROCEDURE ProcessLine(Line: Name;
                       FileID: CARDINAL;
                       LineNumber: CARDINAL);
VAR I: CARDINAL;
    C: CHAR;
    Ref: Ref;
    Label: Name;
BEGIN
    FOR I := 0 TO Length(Line) - 1 DO
        C := Line[I];
        IF C = ':' THEN
            Copy(Line, I + 1, Length(Line) - I, Label);
            IF NOT EqString(Label, ['.']) THEN
                Ref.File := Vectors.Index(Files, FileID);
                Ref.Line := LineNumber;
                RefCount(Vectors.Index(CrossRefs, I)) := RefCount(Vectors.Index(CrossRefs, I)) + 1;
                Append(Vectors.Index(CrossRefs, I).Refs, Ref)
            END
        END
    END
END ProcessLine;

PROCEDURE ProcessFile(Name: Name);
VAR FileID: CARDINAL;
    Timestamp: LONGINT;
    File, Line: Name;
    LineNumber: CARDINAL;
BEGIN
    FileID := Open(Name, Create + Write);
    IF FileID < 0 THEN
        PrintStringNL("Cannot open file: ");
        PrintStringNL(Name);
        RETURN
    END;
    LineNumber := 0;
    WHILE NOT EoF(FileID) DO
        LineNumber := LineNumber + 1;
        Read(FileID, File);
        ProcessLine(File, FileID, LineNumber)
    END;
    TimeStamp(Timestamp);
    Close(FileID);
    Write(FileID, ToLongInt(Timestamp))
END ProcessFile;

PROCEDURE BuildCrossRef;
VAR I, J: CARDINAL;
    C: Cross;
    Ref: Ref;
BEGIN
    PrintStringNL("Building cross-reference table...");
    FOR I := 0 TO Vectors.Size(Files) - 1 DO
        ProcessFile(Vectors.Index(Files, I))
    END;
    PrintStringNL("Sorting cross-reference table...");
    FOR I := 0 TO Vectors.Size(CrossRefs) - 1 DO
        FOR J := I + 1 TO Vectors.Size(CrossRefs) - 1 DO
            C := Vectors.Index(CrossRefs, I);
            IF C.Label > Vectors.Index(CrossRefs, J).Label THEN
                Vectors.DestructiveReplace(CrossRefs, I, Vectors.Index(CrossRefs, J));
                Vectors.DestructiveReplace(CrossRefs, J, C)
            END
        END
    END
END BuildCrossRef;

PROCEDURE PrintCrossRef;
VAR I, J: CARDINAL;
    C: Cross;
    R: Ref;
BEGIN
    PrintStringNL("Label           File                      Line");
    PrintStringNL("--------------------------------------------");
    FOR I := 0 TO Vectors.Size(CrossRefs) - 1 DO
        C := Vectors.Index(CrossRefs, I);
        PrintString(C.Label);
        FOR J := 0 TO C.RefCount - 1 DO
            R := C.Refs[J];
            PrintString(" ");
            PrintString(R.File);
            PrintNum(R.Line)
        END
    END
END PrintCrossRef;

BEGIN
    Init;
    Cls;
    GetChar;

    AddFile("CROSSREF.PAS");
    AddFile("FILEIO.PAS");
    AddFile("SEARCH.PAS");
    AddFile("SORT.PAS");
    AddFile("STRING.PAS");
    AddFile("TARGET.PAS");
    AddFile("TERMINAL.PAS");
    AddFile("VECTORS.PAS");

    BuildCrossRef;
    PrintCrossRef;
    Free;

    NewLine;
    PrintStringNL("Press any key to exit.");
    GetChar
END CrossRef.
```

This Modula-2 code generates a cross-reference table for a set of source code files.

The program first initializes the data structures it will use to store the information about the files and the cross-references.

Then, it enters a loop to process each source code file. For each file, it opens the file and reads it line by line. As it reads each line, it looks for references to other source code files. When it finds a reference, it adds it to the cross-reference table.

Once the program has processed all of the source code files, it sorts the cross-reference table alphabetically by label.

Finally, it prints the cross-reference table to the console.

Here is a brief explanation of the code:

* The `Init` procedure initializes the data structures used by the program.
* The `Free` procedure frees the memory used by the data structures.
* The `AddFile` procedure adds a new file to the list of files to be processed.
* The `ProcessLine` procedure processes a single line of source code, looking for references to other source code files.
* The `ProcessFile` procedure processes a single source code file.
* The `BuildCrossRef` procedure builds the cross-reference table.
* The `PrintCrossRef` procedure prints the cross-reference table to the console.

This program is a good example of how to use Modula-2 to process text files and generate reports.