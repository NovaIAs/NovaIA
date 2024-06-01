**Section de données :**

```assembly
; Déclaration des étiquettes de saut
Debut:
Fin:
```

**Section de code :**

```assembly
; Point d'entrée du programme
Debut:

; Chargement d'une valeur dans le registre AX
MOV AX, 1000h

; Boucle 1 : Affichage de la valeur du registre AX
Boucle1:
    ; Affichage de la valeur du registre AX
    MOV BX, AX
    CALL AfficheNombre

    ; Incrémentation de la valeur du registre AX
    INC AX

    ; Test de la condition de fin de boucle
    CMP AX, 1500h
    JNB FinBoucle1

    ; Saut au début de la boucle
    JMP Boucle1

FinBoucle1:

; Boucle 2 : Calcul factorielle
Boucle2:
    ; Chargement de la valeur du registre AX dans le registre BX
    MOV BX, AX

    ; Calcul factorielle
    CALL CalculeFactorielle

    ; Affichage de la valeur du registre BX
    MOV BX, AX
    CALL AfficheNombre

    ; Incrémentation de la valeur du registre AX
    INC AX

    ; Test de la condition de fin de boucle
    CMP AX, 10
    JNB FinBoucle2

    ; Saut au début de la boucle
    JMP Boucle2

FinBoucle2:

; Saut à la fin du programme
JMP Fin

; Sous-programme d'affichage d'un nombre
AfficheNombre:
    ; Sauvegarde des registres
    PUSH BX
    PUSH DS

    ; Affichage du nombre
    MOV AH, 2
    MOV DL, BL
    INT 21h

    ; Restauration des registres
    POP DS
    POP BX

    ; Retour au programme principal
    RET

; Sous-programme de calcul de factorielle
CalculeFactorielle:
    ; Sauvegarde des registres
    PUSH BX
    PUSH DX

    ; Initialisation du registre DX à 1
    MOV DX, 1

    ; Boucle de calcul de factorielle
BoucleFactorielle:
        ; Multiplication du registre DX par le registre BX
        MUL BX

        ; Décrémentation du registre BX
        DEC BX

        ; Test de la condition de fin de boucle
        JZ FinBoucleFactorielle

        ; Saut au début de la boucle
        JMP BoucleFactorielle

FinBoucleFactorielle:

    ; Restauration des registres
    POP DX
    POP BX

    ; Retour au programme principal
    RET

; Fin du programme
Fin:
    ; Arrêt du programme
    INT 20h
```

**Explication :**

Ce code en Assembly effectue deux tâches principales :

1. **Affichage d'une séquence de nombres de 1000 à 1500** : Il utilise une boucle pour incrémenter le registre AX de 1 à chaque itération et afficher la valeur du registre à l'écran via le sous-programme `AfficheNombre`.

2. **Calcul de la factorielle des nombres de 1 à 10** : Il utilise une deuxième boucle pour incrémenter le registre AX de 1 à chaque itération et appeler le sous-programme `CalculeFactorielle` pour calculer la factorielle du nombre stocké dans AX. La factorielle est ensuite affichée à l'écran.

Le code utilise des sous-programmes pour encapsuler la fonctionnalité d'affichage et de calcul factorielle, rendant le code plus modulaire et réutilisable. Les registres sont soigneusement sauvegardés et restaurés pour préserver l'état du programme.

Ce code est complexe et difficile à reproduire en raison de sa structure multi-boucles, de l'utilisation de sous-programmes et de la manipulation de registres à bas niveau.