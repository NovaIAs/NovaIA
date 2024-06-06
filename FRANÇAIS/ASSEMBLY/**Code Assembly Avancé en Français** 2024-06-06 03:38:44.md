**Code Assembly Complexe en Français**

**Section des Données**

```assembly
SEGMENT DATA

MESSAGE: DB "Bonjour, monde !"
LONGUEUR: EQU $-MESSAGE

;; Tableau d'entiers
TABLE: DD 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
LONGUEUR_TABLE: EQU $ - TABLE

;; Zone de mémoire tampon
BUFFER: TIMES 128 DB 0 ;; 128 octets de mémoire tampon

SEGMENT ENDS
```

**Section du Code**

```assembly
SEGMENT CODE

DEMARRAGE:
    ;; Affiche le message "Bonjour, monde !"
    MOV AH, 9
    LEA DX, MESSAGE
    INT 21H

    ;; Calcule la moyenne des éléments du tableau
    MOV BX, 0 ;; Indice de l'élément actuel
    MOV CX, 0 ;; Accumulateur de la somme
    MOV DX, LONGUEUR_TABLE ;; Nombre d'éléments

BOUCLE_TABLE:
    MOV AX, TABLE[BX] ;; Charge l'élément actuel
    ADD CX, AX ;; Ajoute l'élément à la somme
    INC BX ;; Incrémente l'indice
    LOOP BOUCLE_TABLE

    DIV DX ;; Divise la somme par le nombre d'éléments
    MOV SI, CX ;; Enregistre la moyenne dans SI

    ;; Affiche la moyenne
    MOV AH, 2
    LEA DX, BUFFER
    INT 21H

    MOV AL, BUFFER[0] ;; Récupère le premier caractère de la moyenne
    ADD AL, 48 ;; Convertit le chiffre en code ASCII
    MOV BUFFER[0], AL ;; Stocke le chiffre dans la mémoire tampon

    MOV AH, 2
    MOV DL, ',' ;; Affiche une virgule
    INT 21H

    MOV AL, BUFFER[1] ;; Récupère le deuxième caractère de la moyenne
    ADD AL, 48 ;; Convertit le chiffre en code ASCII
    MOV BUFFER[1], AL ;; Stocke le chiffre dans la mémoire tampon

    MOV AH, 2
    MOV DL, '\n' ;; Affiche un saut de ligne
    INT 21H

    ;; Attend une touche
    MOV AH, 1
    INT 21H

    ;; Termine le programme
    MOV AH, 4CH
    INT 21H

CODE ENDS
```

**Explication du Code**

**Section des Données**

* `MESSAGE` : Chaîne de caractères contenant le message "Bonjour, monde !".
* `LONGUEUR` : Longueur de la chaîne de caractères.
* `TABLE` : Tableau d'entiers.
* `LONGUEUR_TABLE` : Longueur du tableau.
* `BUFFER` : Zone de mémoire tampon utilisée pour stocker la moyenne.

**Section du Code**

* `DEMARRAGE` : Point d'entrée du programme.
* La première partie du code affiche le message "Bonjour, monde !" à l'écran.
* La deuxième partie calcule la moyenne des éléments du tableau `TABLE`.
* La troisième partie affiche la moyenne à l'aide de la zone de mémoire tampon `BUFFER` et la fonction `INT 21H`.
* Le programme attend une touche avant de terminer.