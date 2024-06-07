**Code complexe en Visual Basic**

Ce code effectue les opérations suivantes :

* Crée une interface graphique utilisateur avec un formulaire, deux boutons et une zone de texte
* Lit un fichier texte et stocke son contenu dans un tableau
* Analyse chaque élément du tableau pour trouver les nombres entiers
* Affiche la somme et la moyenne des nombres entiers dans la zone de texte

**Code :**

```visual basic
Imports System.IO

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim path = "fichier.txt"
        Dim lines = File.ReadAllLines(path)

        Dim tab As String() = lines

        Dim sum = 0
        Dim count = 0

        For i As Integer = 0 To tab.GetLength(0) - 1
            Dim isInteger As Boolean
            If Int32.TryParse(tab(i), isInteger) Then
                sum += Int32.Parse(tab(i))
                count += 1
            End If
        Next

        TextBox1.Text = "Somme : " & sum.ToString() & vbCrLf & "Moyenne : " & (sum / count).ToString()
    End Sub

End Class
```

**Explication du code :**

* La ligne `Imports System.IO` importe la bibliothèque d'entrée/sortie de fichiers de Visual Basic.
* Le bouton `Button1` est lié à l'événement `Click`.
* La variable `path` stocke le chemin d'accès au fichier texte.
* La ligne `Dim lines = File.ReadAllLines(path)` lit le fichier texte et stocke son contenu dans un tableau de chaînes.
* La ligne `Dim tab As String() = lines` convertit le tableau de chaînes en tableau de chaînes.
* Les variables `sum` et `count` stockent la somme et le nombre de nombres entiers trouvés, respectivement.
* La boucle `For` itère sur chaque élément du tableau.
* La ligne `If Int32.TryParse(tab(i), isInteger) Then` essaie d'analyser l'élément actuel en nombre entier.
* Si l'élément est un nombre entier, il est ajouté à `sum` et `count` est incrémenté.
* Enfin, la somme et la moyenne des nombres entiers sont affichées dans la zone de texte `TextBox1`.