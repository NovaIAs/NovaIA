**Code complexe en Visual Basic**

```visual basic
' Importer les bibliothèques nécessaires
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text

' Définir une classe générique
Class Paire(Of T)
    ' Les propriétés de la paire
    Public Property Premier As T
    Public Property Second As T

    ' Constructeur
    Public Sub New(premier As T, second As T)
        Premier = premier
        Second = second
    End Sub

    ' Méthode d'affichage
    Public Overrides Function ToString() As String
        Return $"({Premier}, {Second})"
    End Function
End Class

' Définir une classe statique pour les méthodes utilitaires
Module Utilitaires

    ' Méthode pour trouver les nombres premiers dans un intervalle
    Public Function TrouverNombresPremiers(debut As Integer, fin As Integer) As List(Of Integer)
        ' Initialiser la liste des nombres premiers
        Dim nombresPremiers As New List(Of Integer)

        ' Itérer sur l'intervalle
        For i As Integer = debut To fin
            ' Vérifier si le nombre est premier
            If EstPremier(i) Then
                ' Ajouter le nombre à la liste
                nombresPremiers.Add(i)
            End If
        Next

        ' Renvoyer la liste des nombres premiers
        Return nombresPremiers
    End Function

    ' Méthode pour vérifier si un nombre est premier
    Public Function EstPremier(nombre As Integer) As Boolean
        ' Vérifier si le nombre est négatif ou inférieur à 1
        If nombre < 2 Then
            Return False
        End If

        ' Itérer sur les diviseurs potentiels
        For i As Integer = 2 To Math.Floor(nombre / 2)
            ' Vérifier si le nombre est divisible par le diviseur potentiel
            If nombre Mod i = 0 Then
                ' Le nombre n'est pas premier
                Return False
            End If
        Next

        ' Le nombre est premier
        Return True
    End Function

End Module

' Point d'entrée du programme
Sub Main()
    ' Créer une liste de paires de nombres premiers
    Dim pairesDeNombresPremiers As New List(Of Paire(Of Integer))

    ' Obtenir les nombres premiers dans l'intervalle [1, 100]
    Dim nombresPremiers = Utilitaires.TrouverNombresPremiers(1, 100)

    ' Créer des paires de nombres premiers
    For i As Integer = 0 To nombresPremiers.Count - 1
        For j As Integer = i + 1 To nombresPremiers.Count - 1
            ' Créer une paire
            Dim paire = New Paire(Of Integer)(nombresPremiers(i), nombresPremiers(j))

            ' Ajouter la paire à la liste
            pairesDeNombresPremiers.Add(paire)
        Next
    Next

    ' Afficher la liste des paires de nombres premiers
    For Each paire In pairesDeNombresPremiers
        ' Afficher la paire
        Console.WriteLine(paire.ToString())
    Next
End Sub
```

**Explication du code :**

* **Classe Paire** : Une classe générique qui représente une paire d'éléments.
* **Module Utilitaires** : Un module statique contenant des méthodes utilitaires, notamment la recherche de nombres premiers.
* **Fonction TrouverNombresPremiers** : Trouve les nombres premiers dans un intervalle donné.
* **Fonction EstPremier** : Vérifie si un nombre donné est premier.
* **Boucle principale** :
    * Crée une liste de paires de nombres premiers.
    * Utilise la fonction TrouverNombresPremiers pour obtenir les nombres premiers dans l'intervalle [1, 100].
    * Crée des paires de nombres premiers à partir des nombres obtenus.
    * Affiche la liste des paires de nombres premiers.

Ce code crée une liste de toutes les paires possibles de nombres premiers dans l'intervalle [1, 100].