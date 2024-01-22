```visual basic
' Esta macro crea una hoja de cálculo de Excel con múltiples hojas de trabajo y las llena con datos aleatorios.

' Crear un nuevo libro de trabajo
Workbooks.Add

' Agregar tres hojas de trabajo a un libro de trabajo
Sheets.Add
Sheets.Add
Sheets.Add

' Dar nombre a las hojas de trabajo
With Worksheets(1)
    .Name = "Hoja1"
End With

With Worksheets(2)
    .Name = "Hoja2"
End With

With Worksheets(3)
    .Name = "Hoja3"
End With

' Rellenar las hojas de trabajo con datos aleatorios
For i = 1 To 100
    Worksheets("Hoja1").Cells(i, 1).Value = Rnd * 100
    Worksheets("Hoja2").Cells(i, 1).Value = Rnd * 100
    Worksheets("Hoja3").Cells(i, 1).Value = Rnd * 100
Next i

' Guardar el libro de trabajo como "libro.xlsx"
ActiveWorkbook.SaveAs "libro.xlsx"

' Cerrar el libro de trabajo
ActiveWorkbook.Close

' Mostrar un mensaje de confirmación
MsgBox "El libro de trabajo se ha creado y guardado."

' Explicación del código:

' 1. Crear un nuevo libro de trabajo:
'    Workbooks.Add

' 2. Agregar tres hojas de trabajo al libro de trabajo:
'    Sheets.Add

' 3. Dar nombre a las hojas de trabajo:
'    Worksheets(1).Name = "Hoja1"
'    Worksheets(2).Name = "Hoja2"
'    Worksheets(3).Name = "Hoja3"

' 4. Rellenar las hojas de trabajo con datos aleatorios:
'    For i = 1 To 100
'        Worksheets("Hoja1").Cells(i, 1).Value = Rnd * 100
'        Worksheets("Hoja2").Cells(i, 1).Value = Rnd * 100
'        Worksheets("Hoja3").Cells(i, 1).Value = Rnd * 100
'    Next i

' 5. Guardar el libro de trabajo como "libro.xlsx":
'    ActiveWorkbook.SaveAs "libro.xlsx"

' 6. Cerrar el libro de trabajo:
'    ActiveWorkbook.Close

' 7. Mostrar un mensaje de confirmación:
'    MsgBox "El libro de trabajo se ha creado y guardado."
```