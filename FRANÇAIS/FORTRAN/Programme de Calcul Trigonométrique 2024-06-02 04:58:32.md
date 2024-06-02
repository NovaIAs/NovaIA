```fortran
MODULE MODULE_DONNEES_CONSTANTES
IMPLICIT NONE

! Constantes globales
CHARACTER(LEN=40) :: MSG_COMPILATION = "Compilation réussie !"
CHARACTER(LEN=40) :: MSG_EXECUTION = "Exécution terminée !"
LOGICAL :: BOOL_VRAI = .TRUE.
LOGICAL :: BOOL_FAUX = .FALSE.

END MODULE MODULE_DONNEES_CONSTANTES

MODULE MODULE_ENTREE_SORTIE
IMPLICIT NONE

! Procédures d'entrée/sortie

CONTAINS

SUBROUTINE IMPRIMER_MESSAGE (MESSAGE)
IMPLICIT NONE

! Impression d'un message à l'écran

CHARACTER(LEN=*) :: MESSAGE

WRITE (*, *) MESSAGE

END SUBROUTINE IMPRIMER_MESSAGE

END MODULE MODULE_ENTREE_SORTIE

MODULE MODULE_FONCTIONS_MATHEMATIQUES
IMPLICIT NONE

! Fonctions mathématiques

CONTAINS

FUNCTION SINUS (X)
IMPLICIT NONE

! Calcul du sinus de X

REAL :: X
REAL :: RESULTAT

RESULTAT = SIN(X)

RETURN
END FUNCTION SINUS

FUNCTION COSINUS (X)
IMPLICIT NONE

! Calcul du cosinus de X

REAL :: X
REAL :: RESULTAT

RESULTAT = COS(X)

RETURN
END FUNCTION COSINUS

END MODULE MODULE_FONCTIONS_MATHEMATIQUES

MODULE MODULE_PROGRAMME_PRINCIPAL
IMPLICIT NONE

! Programme principal

CONTAINS

SUBROUTINE PROGRAMME_PRINCIPAL
IMPLICIT NONE

! Déclaration des variables locales
REAL :: ANGLE_RADIANS = 0.0
REAL :: ANGLE_DEGRES = 0.0
REAL :: SINUS_ANGLE = 0.0
REAL :: COSINUS_ANGLE = 0.0

! Affichage d'un message de compilation
MODULE_ENTREE_SORTIE => IMPRIMER_MESSAGE(MODULE_DONNEES_CONSTANTES => MSG_COMPILATION)

! Saisie de l'angle en degrés
READ (*, *) ANGLE_DEGRES

! Conversion de l'angle en radians
ANGLE_RADIANS = ANGLE_DEGRES * 3.141592653589793 / 180.0

! Calcul du sinus et du cosinus de l'angle
SINUS_ANGLE = MODULE_FONCTIONS_MATHEMATIQUES => SINUS(ANGLE_RADIANS)
COSINUS_ANGLE = MODULE_FONCTIONS_MATHEMATIQUES => COSINUS(ANGLE_RADIANS)

! Affichage des résultats
MODULE_ENTREE_SORTIE => IMPRIMER_MESSAGE("Sinus de " & TRIM(ADJUSTL(ANGLE_DEGRES, 6)) & " degrés : " & TRIM(ADJUSTL(SINUS_ANGLE, 6)))
MODULE_ENTREE_SORTIE => IMPRIMER_MESSAGE("Cosinus de " & TRIM(ADJUSTL(ANGLE_DEGRES, 6)) & " degrés : " & TRIM(ADJUSTL(COSINUS_ANGLE, 6)))

! Affichage d'un message d'exécution
MODULE_ENTREE_SORTIE => IMPRIMER_MESSAGE(MODULE_DONNEES_CONSTANTES => MSG_EXECUTION)

END SUBROUTINE PROGRAMME_PRINCIPAL

END MODULE MODULE_PROGRAMME_PRINCIPAL

PROGRAM PROGRAMME_CALCUL_TRIGONOMETRIE
IMPLICIT NONE

! Appel du programme principal
MODULE_PROGRAMME_PRINCIPAL => PROGRAMME_PRINCIPAL

END PROGRAM PROGRAMME_CALCUL_TRIGONOMETRIE
```

**Explications du code :**

Ce code FORTRAN complexe est un programme qui calcule le sinus et le cosinus d'un angle donné en degrés. Le code est organisé en plusieurs modules :

* **MODULE_DONNEES_CONSTANTES** : Contient des constantes globales, telles que des messages texte et des valeurs booléennes.
* **MODULE_ENTREE_SORTIE** : Contient des procédures d'entrée/sortie pour afficher des messages à l'écran et lire des entrées depuis l'utilisateur.
* **MODULE_FONCTIONS_MATHEMATIQUES** : Contient des fonctions mathématiques pour calculer le sinus et le cosinus.
* **MODULE_PROGRAMME_PRINCIPAL** : Contient le programme principal qui appelle les autres modules et effectue les calculs.

**Déroulement du programme :**

1. Le programme affiche un message de compilation réussi.
2. Il demande à l'utilisateur de saisir un angle en degrés.
3. Il convertit l'angle en radians.
4. Il calcule le sinus et le cosinus de l'angle à l'aide des fonctions mathématiques.
5. Il affiche les résultats à l'écran.
6. Enfin, il affiche un message d'exécution terminée.

Ce code FORTRAN est complexe en raison de son organisation modulaire, de sa gestion des constantes globales et de ses fonctions mathématiques. Il est un exemple de code FORTRAN structuré et bien organisé.