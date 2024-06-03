**Module de conversion de devises**

Ce module fournit des fonctions pour convertir entre différentes devises.

```visual basic
' Déclaration des constantes des taux de change
Const TAUX_EURO_US = 1,08
Const TAUX_US_EURO = 0,93
Const TAUX_GBP_US = 1,25
Const TAUX_US_GBP = 0,80

' Fonction de conversion euro vers dollar US
Function EuroToUS(montant As Double) As Double
    Return montant * TAUX_EURO_US
End Function

' Fonction de conversion dollar US vers euro
Function USToEuro(montant As Double) As Double
    Return montant * TAUX_US_EURO
End Function

' Fonction de conversion livre sterling vers dollar US
Function GBPToUS(montant As Double) As Double
    Return montant * TAUX_GBP_US
End Function

' Fonction de conversion dollar US vers livre sterling
Function USToGBP(montant As Double) As Double
    Return montant * TAUX_US_GBP
End Function
```

**Utilisation du module de conversion de devises**

Pour utiliser le module, créez une instance du module dans votre code.

```visual basic
Dim convertisseur As New ModuleConversionDevises
```

Ensuite, vous pouvez utiliser les fonctions du module pour convertir entre les devises.

```visual basic
' Convertir 100 euros en dollars US
Dim montantUS As Double = convertisseur.EuroToUS(100)

' Convertir 200 dollars US en euros
Dim montantEuro As Double = convertisseur.USToEuro(200)
```

**Explication du code**

* Les constantes sont utilisées pour stocker les taux de change.
* Les fonctions de conversion utilisent les constantes pour convertir les montants entre les devises.
* L'instance du module est créée à l'aide de la commande `New`.
* Les fonctions du module sont appelées en utilisant le nom de l'instance, suivi d'un point et du nom de la fonction.