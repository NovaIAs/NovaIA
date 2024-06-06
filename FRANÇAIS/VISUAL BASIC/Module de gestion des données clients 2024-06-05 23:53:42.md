**Module de gestion des données client**

```visual basic
Option Explicit

' Déclaration des constantes
Const NB_CLIENTS = 100 ' Nombre maximum de clients

' Déclaration du type Client
Type Client
    Nom As String
    Prenom As String
    Adresse As String
    Telephone As String
    Email As String
End Type

' Déclaration du tableau de clients
Dim Clients() As Client

' Procédure de chargement des données client depuis un fichier texte
Sub ChargerClients()
    Dim Fich As Integer
    Dim Ligne As String

    ' Ouverture du fichier texte
    Fich = FreeFile
    Open "clients.txt" For Input As #Fich

    ' Lecture du fichier ligne par ligne
    Do Until EOF(Fich)
        Ligne = Input$(LOF(Fich), #Fich)
        ' Traitement de la ligne
        Dim C As Client
        C.Nom = Left(Ligne, 20)
        C.Prenom = Mid(Ligne, 21, 15)
        C.Adresse = Mid(Ligne, 36, 50)
        C.Telephone = Mid(Ligne, 86, 15)
        C.Email = Mid(Ligne, 101, 50)

        ' Ajout du client au tableau
        ReDim Preserve Clients(UBound(Clients) + 1)
        Clients(UBound(Clients)) = C
    Loop

    ' Fermeture du fichier texte
    Close #Fich
End Sub

' Procédure d'affichage des données client
Sub AfficherClients()
    For i = 0 To UBound(Clients)
        Debug.Print "Nom :" & Clients(i).Nom
        Debug.Print "Prénom :" & Clients(i).Prenom
        Debug.Print "Adresse :" & Clients(i).Adresse
        Debug.Print "Téléphone :" & Clients(i).Telephone
        Debug.Print "Email :" & Clients(i).Email
        Debug.Print "----------------------------------------"
    Next i
End Sub

' Procédure principale
Sub Main()
    ' Chargement des données client
    ChargerClients()

    ' Affichage des données client
    AfficherClients()
End Sub
```

**Explication du code**

Ce code implémente un module de gestion de données client en Visual Basic.

* Il définit une constante `NB_CLIENTS` pour limiter le nombre de clients à 100.
* Il définit un type `Client` pour représenter les données d'un client.
* Il déclare un tableau `Clients` pour stocker les données des clients.
* La procédure `ChargerClients` charge les données client depuis un fichier texte.
* La procédure `AfficherClients` affiche les données client dans la fenêtre de débogage.
* La procédure `Main` est le point d'entrée du code. Elle appelle `ChargerClients` pour charger les données client et `AfficherClients` pour les afficher.