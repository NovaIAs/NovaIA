```visual basic
' Les procédures et fonctions :

' Procédure qui affiche un message dans une boîte de message.
Public Sub AfficheMessage(titre As String, message As String)
    MsgBox message, vbOKOnly, titre
End Sub

' Fonction qui retourne la valeur absolue d'un nombre.
Public Function ValeurAbsolue(nombre As Double) As Double
    If nombre < 0 Then
        ValeurAbsolue = -nombre
    Else
        ValeurAbsolue = nombre
    End If
End Function

' Les modules :

' Module contenant des constantes et des variables globales.
Module ConstantesEtVariables

    ' Constantes
    Public Const TAUREAU = "Taurus"
    Public Const GEMEAUX = "Gemini"

    ' Variables
    Public age As Integer

End Module

' Module contenant les classes.
Module Classes

    ' Classe Personne
    Class Personne

        ' Attributs
        Private nom As String
        Private prenom As String

        ' Constructeur
        Public Sub New(nom As String, prenom As String)
            Me.nom = nom
            Me.prenom = prenom
        End Sub

        ' Méthodes
        Public Function GetNom() As String
            GetNom = nom
        End Function

        Public Function GetPrenom() As String
            GetPrenom = prenom
        End Function

        Public Sub SetNom(nouveauNom As String)
            nom = nouveauNom
        End Sub

        Public Sub SetPrenom(nouveauPrenom As String)
            prenom = nouveauPrenom
        End Sub

        Public Sub AfficheNomPrenom()
            AfficheMessage("Nom et prénom", Me.GetNom() & " " & Me.GetPrenom())
        End Sub

    End Class

    ' Classe CompteBancaire
    Class CompteBancaire

        ' Attributs
        Private solde As Double
        Private numeroCompte As String

        ' Constructeur
        Public Sub New(numeroCompte As String, solde Initial As Double)
            Me.numeroCompte = numeroCompte
            Me.solde = solde Initial
        End Sub

        ' Méthodes
        Public Function GetSolde() As Double
            GetSolde = solde
        End Function

        Public Function GetNumeroCompte() As String
            GetNumeroCompte = numeroCompte
        End Function

        Public Sub SetSolde(nouveauSolde As Double)
            solde = nouveauSolde
        End Sub

        Public Sub Debiter(montant As Double)
            If montant <= Me.GetSolde() Then
                Me.SetSolde(Me.GetSolde() - montant)
            Else
                AfficheMessage("Erreur", "Le montant à débiter est supérieur au solde disponible.")
            End If
        End Sub

        Public Sub Crediter(montant As Double)
            Me.SetSolde(Me.GetSolde() + montant)
        End Sub

        Public Sub AfficheSolde()
            AfficheMessage("Solde du compte", Me.GetNumeroCompte() & " : " & Me.GetSolde())
        End Sub

    End Class

End Module

' Le code principal :

Option Explicit

Private Sub Form_Load()

    ' Créer une nouvelle personne.
    Dim p1 As New Personne("Dupont", "Jean")

    ' Afficher le nom et le prénom de la personne.
    p1.AfficheNomPrenom()

    ' Créer un nouveau compte bancaire.
    Dim cb1 As New CompteBancaire("FR123456789012345", 1000)

    ' Afficher le solde du compte bancaire.
    cb1.AfficheSolde()

    ' Débiter le compte bancaire.
    cb1.Debiter(500)

    ' Afficher à nouveau le solde du compte bancaire.
    cb1.AfficheSolde()

    ' Créditer le compte bancaire.
    cb1.Crediter(1000)

    ' Afficher une nouvelle fois le solde du compte bancaire.
    cb1.AfficheSolde()

End Sub
```

**Explication du code :**

**Les procédures et fonctions :**

* La procédure `AfficheMessage` crée une boîte de message affichant le `message` et le `titre` spécifiés.
* La fonction `ValeurAbsolue` retourne la valeur absolue d’un `nombre`.

**Les modules :**

* Le module `ConstantesEtVariables` contient les constantes et les variables globales.
* Le module `Classes` contient les définitions des classes.

**Les classes :**

* La classe `Personne` représente une personne avec un `nom` et un `prenom`.
* La classe `CompteBancaire` représente un compte bancaire avec un `numeroCompte` et un `solde`.

**Le code principal :**

* Le formulaire est chargé.
* Une instance de la classe `Personne` est créée.
* La méthode `AfficheNomPrenom` de l’instance de la classe `Personne` est appelée.
* Une instance de la classe `CompteBancaire` est créée.
* La méthode `AfficheSolde` de l’instance de la classe `CompteBancaire` est appelée.
* La méthode `Debiter` de l’instance de la classe `CompteBancaire` est appelée.
* La méthode `AfficheSolde` de l’instance de la classe `CompteBancaire` est appelée à nouveau.
* La méthode `Crediter` de l’instance de la classe `CompteBancaire` est appelée.
* La méthode `AfficheSolde` de l’instance de la classe `CompteBancaire` est appelée une dernière fois.