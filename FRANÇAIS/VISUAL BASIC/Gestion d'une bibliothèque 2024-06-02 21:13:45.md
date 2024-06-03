**Module de gestion d'une bibliothèque**

```visual basic
Option Explicit

' Constantes
Const MAX_LIVRES = 1000
Const MAX_EMPRUNTEURS = 100

' Types personnalisés
Type Livre
    Titre As String
    Auteur As String
    ISBN As String
    Exemplaires As Integer
    Empruntés As Integer
End Type

Type Emprunteur
    Nom As String
    Prénom As String
    Adresse As String
    Téléphone As String
    Emprunts En Cours As Collection
    Livres Empruntés As Collection
End Type

' Variables
Dim Livres() As Livre
Dim Emprunteurs() As Emprunteur

' Procédures
Sub ChargerLivres()
    ' Ouvrir le fichier de données des livres
    Dim Fichier As File
    Fichier.Open "Livres.txt", FileMode.Open, FileAccess.Read
    
    ' Lire les données et les stocker dans le tableau Livres
    Dim Ligne As String
    Do While Not Fichier.AtEndOfStream
        Ligne = Fichier.ReadLine
        Dim Colonnes = Split(Ligne, ";")
        Dim LivreCourant = New Livre With { _
            .Titre = Colonnes(0), _
            .Auteur = Colonnes(1), _
            .ISBN = Colonnes(2), _
            .Exemplaires = Colonnes(3), _
            .Empruntés = Colonnes(4) _
        }
        Redim Preserve Livres(I)
        Livres(I) = LivreCourant
    Loop
    
    ' Fermer le fichier
    Fichier.Close()
End Sub

Sub ChargerEmprunteurs()
    ' Ouvrir le fichier de données des emprunteurs
    Dim Fichier As File
    Fichier.Open "Emprunteurs.txt", FileMode.Open, FileAccess.Read
    
    ' Lire les données et les stocker dans le tableau Emprunteurs
    Dim Ligne As String
    Do While Not Fichier.AtEndOfStream
        Ligne = Fichier.ReadLine
        Dim Colonnes = Split(Ligne, ";")
        Dim EmprunteurCourant = New Emprunteur With { _
            .Nom = Colonnes(0), _
            .Prénom = Colonnes(1), _
            .Adresse = Colonnes(2), _
            .Téléphone = Colonnes(3) _
        }
        Redim Preserve Emprunteurs(I)
        Emprunteurs(I) = EmprunteurCourant
    Loop
    
    ' Fermer le fichier
    Fichier.Close()
End Sub

Sub EmprunterLivre(Emprunteur As Emprunteur, Livre As Livre)
    ' Vérifier si le livre est disponible
    If Livre.Exemplaires - Livre.Empruntés <= 0 Then
        MsgBox "Ce livre n'est pas disponible.", vbCritical, "Emprunt impossible"
        Exit Sub
    End If
    
    ' Emprunter le livre
    Emprunteur.Livres Empruntés.Add Livre
    Livre.Empruntés += 1
    
    ' Enregistrer les modifications dans les fichiers de données
    EnregistrerLivres()
    EnregistrerEmprunteurs()
    
    ' Afficher un message de confirmation
    MsgBox "Emprunt effectué avec succès.", vbInformation, "Emprunt réussi"
End Sub

Sub RendreLivre(Emprunteur As Emprunteur, Livre As Livre)
    ' Vérifier si le livre est emprunté par l'emprunteur
    If Not Emprunteur.Livres Empruntés.Contains(Livre) Then
        MsgBox "Cet emprunteur n'a pas emprunté ce livre.", vbCritical, "Retour impossible"
        Exit Sub
    End If
    
    ' Rendre le livre
    Emprunteur.Livres Empruntés.Remove Livre
    Livre.Empruntés -= 1
    
    ' Enregistrer les modifications dans les fichiers de données
    EnregistrerLivres()
    EnregistrerEmprunteurs()
    
    ' Afficher un message de confirmation
    MsgBox "Retour effectué avec succès.", vbInformation, "Retour réussi"
End Sub

Sub EnregistrerLivres()
    ' Ouvrir le fichier de données des livres
    Dim Fichier As File
    Fichier.Open "Livres.txt", FileMode.Create, FileAccess.Write
    
    ' Ecrire les données du tableau Livres dans le fichier
    Dim LivreCourant As Livre
    For Each LivreCourant In Livres
        Fichier.WriteLine(LivreCourant.Titre & ";" & LivreCourant.Auteur & ";" & _
            LivreCourant.ISBN & ";" & LivreCourant.Exemplaires & ";" & LivreCourant.Empruntés)
    Next
    
    ' Fermer le fichier
    Fichier.Close()
End Sub

Sub EnregistrerEmprunteurs()
    ' Ouvrir le fichier de données des emprunteurs
    Dim Fichier As File
    Fichier.Open "Emprunteurs.txt", FileMode.Create, FileAccess.Write
    
    ' Ecrire les données du tableau Emprunteurs dans le fichier
    Dim EmprunteurCourant As Emprunteur
    For Each EmprunteurCourant In Emprunteurs
        Fichier.WriteLine(EmprunteurCourant.Nom & ";" & EmprunteurCourant.Prénom & ";" & _
            EmprunteurCourant.Adresse & ";" & EmprunteurCourant.Téléphone)
    Next
    
    ' Fermer le fichier
    Fichier.Close()
End Sub

```

**Explication du code**

Ce code Visual Basic implémente un système de gestion de bibliothèque comprenant les fonctionnalités suivantes :

* **Chargement des données des livres et des emprunteurs à partir de fichiers texte.**
* **Emprunt et retour de livres par les emprunteurs.**
* **Enregistrement des modifications dans les fichiers texte.**

**Types personnalisés**

Deux types personnalisés sont définis : **Livre** et **Emprunteur**. Ces types contiennent des propriétés pour stocker des informations sur les livres et les emprunteurs, respectivement.

**Procédures**

* **ChargerLivres** : charge les données des livres à partir d'un fichier texte dans le tableau **Livres**.
* **ChargerEmprunteurs** : charge les données des emprunteurs à partir d'un fichier texte dans le tableau **Emprunteurs**.
* **EmprunterLivre** : permet aux emprunteurs d'emprunter des livres.
* **RendreLivre** : permet aux emprunteurs de rendre des livres.
* **EnregistrerLivres** : enregistre les modifications apportées au tableau **Livres** dans un fichier texte.
* **EnregistrerEmprunteurs** : enregistre les modifications apportées au tableau **Emprunteurs** dans un fichier texte.