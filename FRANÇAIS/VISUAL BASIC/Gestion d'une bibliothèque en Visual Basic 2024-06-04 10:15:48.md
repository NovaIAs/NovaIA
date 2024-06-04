**Module de gestion d'une bibliothèque**

```visual basic
Option Explicit

' Constantes
Const NB_MAX_LIVRES = 100 ' Nombre maximum de livres dans la bibliothèque

' Type de données
Type Livre
    titre As String
    auteur As String
    anneeEdition As Integer
    disponible As Boolean
End Type

' Dictionnaire des livres
Dim dicoLivres As Scripting.Dictionary

' Sous-programme de création de la bibliothèque
Sub CreerBibliotheque()

    ' Initialise le dictionnaire
    Set dicoLivres = New Scripting.Dictionary

    ' Ajoute des livres dans le dictionnaire
    dicoLivres.Add "Livre1", New Livre: Livre.titre = "Le Petit Prince": Livre.auteur = "Antoine de Saint-Exupéry": Livre.anneeEdition = 1943: Livre.disponible = True
    dicoLivres.Add "Livre2", New Livre: Livre.titre = "Le Seigneur des anneaux": Livre.auteur = "J.R.R. Tolkien": Livre.anneeEdition = 1954: Livre.disponible = False
    dicoLivres.Add "Livre3", New Livre: Livre.titre = "1984": Livre.auteur = "George Orwell": Livre.anneeEdition = 1949: Livre.disponible = True

End Sub

' Sous-programme d'affichage des livres
Sub AfficherLivres()

    ' Boucle sur le dictionnaire
    For Each livre In dicoLivres

        ' Récupère les informations du livre
        Dim titre As String = livre.titre
        Dim auteur As String = livre.auteur
        Dim anneeEdition As Integer = livre.anneeEdition
        Dim disponible As Boolean = livre.disponible

        ' Affiche les informations
        Debug.Print "Livre : " & titre
        Debug.Print "Auteur : " & auteur
        Debug.Print "Année d'édition : " & anneeEdition
        Debug.Print "Disponible : " & disponible

        ' Séparateur
        Debug.Print "---------------------------------------"

    Next

End Sub

' Sous-programme de recherche de livre par titre
Sub RechercherLivre(titre As String)

    ' Essaye de récupérer le livre
    Dim livre As Livre
    On Error Resume Next
    Set livre = dicoLivres(titre)

    ' Si le livre n'est pas trouvé
    If Err.Number Then
        MsgBox "Le livre '" & titre & "' n'a pas été trouvé.", vbExclamation
        Err.Clear
        Exit Sub
    End If

    ' Récupère les informations du livre
    Dim auteur As String = livre.auteur
    Dim anneeEdition As Integer = livre.anneeEdition
    Dim disponible As Boolean = livre.disponible

    ' Affiche les informations
    MsgBox "Livre : " & titre & vbCrLf & _
           "Auteur : " & auteur & vbCrLf & _
           "Année d'édition : " & anneeEdition & vbCrLf & _
           "Disponible : " & IIf(disponible, "Oui", "Non"), vbInformation

End Sub

' Sous-programme de prêt de livre
Sub PreterLivre(titre As String)

    ' Essaye de récupérer le livre
    Dim livre As Livre
    On Error Resume Next
    Set livre = dicoLivres(titre)

    ' Si le livre n'est pas trouvé
    If Err.Number Then
        MsgBox "Le livre '" & titre & "' n'a pas été trouvé.", vbExclamation
        Err.Clear
        Exit Sub
    End If

    ' Vérifie si le livre est disponible
    If livre.disponible = False Then
        MsgBox "Le livre '" & titre & "' n'est pas disponible.", vbExclamation
        Exit Sub
    End If

    ' Prête le livre
    livre.disponible = False

    ' Affiche un message de confirmation
    MsgBox "Le livre '" & titre & "' a été prêté.", vbInformation

End Sub

' Sous-programme de retour de livre
Sub RetournerLivre(titre As String)

    ' Essaye de récupérer le livre
    Dim livre As Livre
    On Error Resume Next
    Set livre = dicoLivres(titre)

    ' Si le livre n'est pas trouvé
    If Err.Number Then
        MsgBox "Le livre '" & titre & "' n'a pas été trouvé.", vbExclamation
        Err.Clear
        Exit Sub
    End If

    ' Vérifie si le livre n'est pas disponible
    If livre.disponible = True Then
        MsgBox "Le livre '" & titre & "' est déjà disponible.", vbExclamation
        Exit Sub
    End If

    ' Retourne le livre
    livre.disponible = True

    ' Affiche un message de confirmation
    MsgBox "Le livre '" & titre & "' a été retourné.", vbInformation

End Sub

' Point d'entrée du programme
Sub Main()

    ' Crée la bibliothèque
    CreerBibliotheque()

    ' Affiche les livres
    AfficherLivres()

    ' Recherche un livre
    RechercherLivre "Le Seigneur des anneaux"

    ' Prête un livre
    PreterLivre "Livre1"

    ' Retourne un livre
    RetournerLivre "Livre1"

End Sub
```

**Explication du code :**

Ce code crée un module pour gérer une bibliothèque de livres. Il utilise un dictionnaire de type "Livre" pour stocker les livres, avec les propriétés suivantes :

* `titre` : Le titre du livre
* `auteur` : L'auteur du livre
* `anneeEdition` : L'année d'édition du livre
* `disponible` : Un booléen indiquant si le livre est disponible

Le module contient plusieurs sous-programmes pour gérer les livres :

* `CreerBibliotheque()` : Crée la bibliothèque et ajoute des livres initiaux.
* `AfficherLivres()` : Affiche tous les livres de la bibliothèque.
* `RechercherLivre()` : Recherche un livre par son titre et affiche ses informations.
* `PreterLivre()` : Prête un livre en le marquant comme indisponible.
* `RetournerLivre()` : Retourne un livre en le marquant comme disponible.

Le point d'entrée du programme, `Main()`, appelle les sous-programmes pour créer la bibliothèque, afficher les livres, effectuer des recherches, prêter et retourner des livres. Ce code démontre l'utilisation des structures de données, des opérateurs de comparaison et des messages d'invite dans Visual Basic.