**Module 1 : Gestion des données**

```visual basic
Public Enum TypeDonnee
  Entier = 1
  Reel = 2
  Chaine = 3
  Date = 4
  Booleen = 5
End Enum

Public Class Valeur
  Private _type As TypeDonnee
  Private _valeur As Variant

  Public Property Get Type() As TypeDonnee
    Return _type
  End Property

  Public Property Get Valeur() As Variant
    Return _valeur
  End Property

  Public Sub New(ByVal type As TypeDonnee)
    _type = type
  End Sub

  Public Sub Initialiser(ByVal valeur As Variant)
    _valeur = valeur
  End Sub
End Class

Public Class Table
  Private _nom As String
  Private _colonnes As Collection

  Public Property Get Nom() As String
    Return _nom
  End Property

  Public Property Get Colonnes() As Collection
    Return _colonnes
  End Property

  Public Sub New(ByVal nom As String)
    _nom = nom
    _colonnes = New Collection
  End Sub

  Public Sub AjouterColonne(ByVal nom As String, ByVal type As TypeDonnee)
    _colonnes.Add New Valeur(type), nom
  End Sub

  Public Function ObtenirColonne(ByVal nom As String) As Valeur
    If _colonnes.Contains(nom) Then
      Return _colonnes(nom)
    Else
      Return Nothing
    End If
  End Function
End Class
```

**Module 2 : Gestion des requêtes**

```visual basic
Public Class Requete
  Private _table As Table
  Private _filtres As Collection

  Public Property Get Table() As Table
    Return _table
  End Property

  Public Property Get Filtres() As Collection
    Return _filtres
  End Property

  Public Sub New(ByVal table As Table)
    _table = table
    _filtres = New Collection
  End Sub

  Public Sub AjouterFiltre(ByVal nomColonne As String, ByVal operateur As String, ByVal valeur As Variant)
    _filtres.Add New Filtre(nomColonne, operateur, valeur), _filtres.Count + 1
  End Sub

  Public Function Executer() As Collection
    Dim result As New Collection
    Dim ligne As Variant

    For Each ligne In _table.Colonnes
      If Not ligne.Type = TypeDonnee.Chaine Then
        ligne.Valeur = Val(ligne.Valeur)
      End If
    Next

    For Each ligne In _table.Colonnes
      If Filtrer(ligne) Then
        result.Add ligne, result.Count + 1
      End If
    Next

    Return result
  End Function

  Private Function Filtrer(ByVal ligne As Valeur) As Boolean
    Dim filtre As Filtre
    Dim valeur As Variant

    For Each filtre In _filtres
      valeur = ligne.Valeur

      If filtre.NomColonne = ligne.Nom Then
        Select Case filtre.Operateur
          Case "=": If valeur = filtre.Valeur Then Return True
          Case ">": If valeur > filtre.Valeur Then Return True
          Case "<": If valeur < filtre.Valeur Then Return True
          Case ">=": If valeur >= filtre.Valeur Then Return True
          Case "<=": If valeur <= filtre.Valeur Then Return True
          Case "<>": If valeur <> filtre.Valeur Then Return True
        End Select
      End If
    Next

    Return False
  End Function
End Class

Public Class Filtre
  Private _nomColonne As String
  Private _operateur As String
  Private _valeur As Variant

  Public Property Get NomColonne() As String
    Return _nomColonne
  End Property

  Public Property Get Operateur() As String
    Return _operateur
  End Property

  Public Property Get Valeur() As Variant
    Return _valeur
  End Property

  Public Sub New(ByVal nomColonne As String, ByVal operateur As String, ByVal valeur As Variant)
    _nomColonne = nomColonne
    _operateur = operateur
    _valeur = valeur
  End Sub
End Class
```

**Module 3 : Test**

```visual basic
Dim table As New Table("Table1")

table.AjouterColonne("ID", TypeDonnee.Entier)
table.AjouterColonne("Nom", TypeDonnee.Chaine)
table.AjouterColonne("Age", TypeDonnee.Entier)

table.Colonnes("ID").Initialiser(1)
table.Colonnes("Nom").Initialiser("Jean")
table.Colonnes("Age").Initialiser(25)

Dim requete As New Requete(table)

requete.AjouterFiltre("Nom", "=", "Jean")
requete.AjouterFiltre("Age", ">=", 20)

Dim result As Collection = requete.Executer()

For Each ligne In result
  Debug.Print ligne.Nom & " (" & ligne.Age & ")"
Next
```

**Explication du code :**

* **Module 1 : Gestion des données**
  * La classe `Valeur` représente une valeur de données avec son type associé.
  * La classe `Table` représente une table de données avec une collection de colonnes.
  * La collection `Collection` est une collection générique qui peut contenir n'importe quel type d'objet.

* **Module 2 : Gestion des requêtes**
  * La classe `Requete` représente une requête qui peut être exécutée sur une table.
  * La collection `Filtre` est une collection de filtres qui peuvent être ajoutés à une requête.
  * La méthode `Executer` exécute la requête et renvoie une collection de lignes qui répondent aux critères de filtrage.
  * La fonction `Filtrer` détermine si une ligne répond aux critères de filtrage.

* **Module 3 : Test**
  * Ce module crée une table, ajoute des données et des filtres, puis exécute la requête pour récupérer les lignes qui répondent aux critères de filtrage.
  * La méthode `Debug.Print` écrit la sortie dans la fenêtre de sortie.