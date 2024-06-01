```visual basic

' Importation des espaces de noms nécessaires
Imports System
Imports System.Collections.Generic
Imports System.Data
Imports System.Linq

' Définition d'une classe personnalisée pour représenter une personne
Public Class Personne
    ' Propriétés de la classe
    Private _nom As String
    Private _prenom As String
    Private _age As Short

    ' Constructeur de la classe
    Public Sub New(nom As String, prenom As String, age As Short)
        _nom = nom
        _prenom = prenom
        _age = age
    End Sub

    ' Propriétés publiques
    Public Property Nom() As String
        Get
            Return _nom
        End Get
        Set(value As String)
            _nom = value
        End Set
    End Property

    Public Property Prenom() As String
        Get
            Return _prenom
        End Get
        Set(value As String)
            _prenom = value
        End Set
    End Property

    Public Property Age() As Short
        Get
            Return _age
        End Get
        Set(value As Short)
            _age = value
        End Set
    End Property
End Class

' Définition d'une classe personnalisée pour représenter une liste de personnes
Public Class ListePersonnes
    ' Attribut privé de la classe
    Private _personnes As List(Of Personne)

    ' Constructeur de la classe
    Public Sub New()
        _personnes = New List(Of Personne)
    End Sub

    ' Propriétés publiques
    Public Property Personnes() As List(Of Personne)
        Get
            Return _personnes
        End Get
        Set(value As List(Of Personne))
            _personnes = value
        End Set
    End Property

    ' Méthodes publiques
    Public Sub AjouterPersonne(personne As Personne)
        _personnes.Add(personne)
    End Sub

    Public Sub SupprimerPersonne(personne As Personne)
        _personnes.Remove(personne)
    End Sub

    Public Sub TrierParNom()
        ' Tri de la liste de personnes par nom
        _personnes = _personnes.OrderBy(Function(p) p.Nom)
    End Sub

    Public Sub TrierParAge()
        ' Tri de la liste de personnes par âge
        _personnes = _personnes.OrderBy(Function(p) p.Age)
    End Sub
End Class

' Création d'une nouvelle instance de la classe ListePersonnes
Dim listePersonnes As New ListePersonnes()

' Ajout de quelques personnes à la liste
listePersonnes.AjouterPersonne(New Personne("Dupont", "Jean", 35))
listePersonnes.AjouterPersonne(New Personne("Martin", "Marie", 28))
listePersonnes.AjouterPersonne(New Personne("Durand", "Paul", 42))

' Affichage de la liste des personnes avant tri
Console.WriteLine("Avant tri :")
For Each personne In listePersonnes.Personnes
    Console.WriteLine(personne.Nom & " " & personne.Prenom & " (" & personne.Age & " ans)")
Next

' Tri de la liste de personnes par nom
listePersonnes.TrierParNom()

' Affichage de la liste des personnes après tri par nom
Console.WriteLine("Après tri par nom :")
For Each personne In listePersonnes.Personnes
    Console.WriteLine(personne.Nom & " " & personne.Prenom & " (" & personne.Age & " ans)")
Next

' Tri de la liste de personnes par âge
listePersonnes.TrierParAge()

' Affichage de la liste des personnes après tri par âge
Console.WriteLine("Après tri par âge :")
For Each personne In listePersonnes.Personnes
    Console.WriteLine(personne.Nom & " " & personne.Prenom & " (" & personne.Age & " ans)")
Next

' Suppression d'une personne de la liste
listePersonnes.SupprimerPersonne(listePersonnes.Personnes(1))

' Affichage de la liste des personnes après suppression
Console.WriteLine("Après suppression :")
For Each personne In listePersonnes.Personnes
    Console.WriteLine(personne.Nom & " " & personne.Prenom & " (" & personne.Age & " ans)")
Next

Console.ReadLine()

```

**Explication du code :**

Ce code Visual Basic crée deux classes personnalisées, `Personne` et `ListePersonnes`. La classe `Personne` représente une personne avec un nom, un prénom et un âge. La classe `ListePersonnes` représente une liste de personnes, avec des méthodes pour ajouter, supprimer et trier les personnes dans la liste.

Dans la partie principale du code, une instance de la classe `ListePersonnes` est créée et quelques personnes y sont ajoutées. La liste est ensuite triée par nom et par âge, puis les personnes sont affichées dans la console. Enfin, une personne est supprimée de la liste et les personnes restantes sont affichées dans la console.