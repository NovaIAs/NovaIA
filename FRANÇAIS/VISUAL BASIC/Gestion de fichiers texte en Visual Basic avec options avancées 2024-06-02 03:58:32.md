```visual basic

' Module pour gérer les fichiers texte avec différentes options
Module ModuleFichierTexte

    ' Constantes pour les options d'ouverture et de fermeture de fichier
    Const ConstOuvertureFichier_Lecture = 1
    Const ConstOuvertureFichier_Écriture = 2
    Const ConstOuvertureFichier_Ajouter = 3
    Const ConstFermetureFichier_Normal = 0
    Const ConstFermetureFichier_AvecSauvegarde = 1
    Const ConstFermetureFichier_SansSauvegarde = 2

    ' Fonction pour ouvrir un fichier texte
    Function OuvrirFichier(strCheminFichier As String, intOptionOuverture As Integer) As Integer
        ' Déclarer la variable pour le numéro de fichier
        Dim intNumeroFichier As Integer

        ' Vérifier l'option d'ouverture
        Select Case intOptionOuverture
            Case ConstOuvertureFichier_Lecture
                ' Ouvrir le fichier en lecture
                intNumeroFichier = FreeFile
                Open strCheminFichier For Input As #intNumeroFichier
            Case ConstOuvertureFichier_Écriture
                ' Ouvrir le fichier en écriture
                intNumeroFichier = FreeFile
                Open strCheminFichier For Output As #intNumeroFichier
            Case ConstOuvertureFichier_Ajouter
                ' Ouvrir le fichier en ajout
                intNumeroFichier = FreeFile
                Open strCheminFichier For Append As #intNumeroFichier
            Case Else
                ' Afficher une erreur
                MsgBox "Option d'ouverture de fichier invalide", vbCritical, "Erreur"
        End Select

        ' Renvoyer le numéro de fichier
        OuvrirFichier = intNumeroFichier
    End Function

    ' Fonction pour fermer un fichier texte
    Function FermerFichier(intNumeroFichier As Integer, intOptionFermeture As Integer)
        ' Vérifier l'option de fermeture
        Select Case intOptionFermeture
            Case ConstFermetureFichier_Normal
                ' Fermer le fichier normalement
                Close #intNumeroFichier
            Case ConstFermetureFichier_AvecSauvegarde
                ' Fermer le fichier avec sauvegarde
                Call WriteTextFile(intNumeroFichier)
                Close #intNumeroFichier
            Case ConstFermetureFichier_SansSauvegarde
                ' Fermer le fichier sans sauvegarde
                Close #intNumeroFichier
            Case Else
                ' Afficher une erreur
                MsgBox "Option de fermeture de fichier invalide", vbCritical, "Erreur"
        End Select
    End Function

    ' Fonction pour écrire du texte dans un fichier texte
    Function WriteTextFile(intNumeroFichier As Integer)
        ' Déclarer les variables
        Dim strTexte As String
        Dim intLongueurTexte As Integer

        ' Récupérer le texte à écrire
        strTexte = InputBox("Saisissez le texte à écrire dans le fichier :")

        ' Obtenir la longueur du texte
        intLongueurTexte = Len(strTexte)

        ' Écrire le texte dans le fichier
        Print #intNumeroFichier, strTexte;
    End Function

    ' Fonction pour lire du texte dans un fichier texte
    Function ReadTextFile(intNumeroFichier As Integer) As String
        ' Déclarer la variable pour le texte
        Dim strTexte As String

        ' Lire le texte dans le fichier
        strTexte = Input(LOF(intNumeroFichier), #intNumeroFichier)

        ' Renvoyer le texte
        ReadTextFile = strTexte
    End Function

End Module

```

**Explication du code :**

Ce code est un module Visual Basic qui fournit des fonctions pour gérer les fichiers texte. Il inclut les fonctions suivantes :

* **OuvrirFichier** : ouvre un fichier texte en lecture, écriture ou ajout.
* **FermerFichier** : ferme un fichier texte avec ou sans sauvegarde des modifications.
* **WriteTextFile** : écrit du texte dans un fichier texte ouvert.
* **ReadTextFile** : lit le texte d'un fichier texte ouvert.

Le module utilise des constantes pour représenter les options d'ouverture et de fermeture de fichier, ce qui permet de rendre le code plus facile à lire et à maintenir. Les fonctions sont conçues pour être génériques, de sorte qu'elles peuvent être utilisées avec n'importe quel fichier texte.