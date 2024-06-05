**Code en ASSEMBLY (Français)**

```assembly
ORG 0x100

; Initialiser les variables
    MOV R0, #0 ; Compteur
    MOV R1, #0x10 ; Limite supérieure
    MOV R2, #0x0A ; Limite inférieure

; Boucle principale
    CMP R0, R1
    BEQ FinBoucle ; Si le compteur atteint la limite supérieure, sortir de la boucle
    CMP R0, R2
    BLT DébutBoucle ; Si le compteur est inférieur à la limite inférieure, revenir au début de la boucle

; Incrémenter le compteur
    ADD R0, #1

; Afficher le compteur sur l'écran
    CALL AfficheCompteur

; Boucler jusqu'à ce que le compteur atteigne la limite supérieure
    JMP BouclePrincipale

; Fin de la boucle
FinBoucle:
    HALT ; Arrêter le programme

; Sous-programme pour afficher le compteur sur l'écran
AfficheCompteur:
    MOV R3, #0x80 ; Registre de sortie
    MOV R4, #0x01 ; Numéro de registre de sortie
    MOV R5, #0x02 ; Mode d'adressage (registre indirect)

    STR R0, [R3], R5 ; Mettre le compteur dans le registre de sortie

    MOV R3, #0x84 ; Registre d'entrée
    MOV R4, #0x01 ; Numéro de registre d'entrée
    MOV R5, #0x02 ; Mode d'adressage (registre indirect)

    LDR R0, [R3], R5 ; Récupérer le compteur du registre d'entrée

    ; Convertir le compteur en chaîne ASCII
    MOV R6, #0x30 ; Décalage pour les caractères ASCII des chiffres
    ADD R0, R6 ; Ajouter le décalage au compteur

    ; Afficher le compteur sur l'écran
    MOV R3, #0x10 ; Registre de sortie
    MOV R4, #0x00 ; Numéro de registre de sortie
    MOV R5, #0x01 ; Mode d'adressage (adressage direct)

    STR R0, [R3], R5 ; Mettre le compteur dans le registre de sortie

    RET

```

**Explication du code:**

Ce code ASSEMBLY implémente une boucle qui incrémente un compteur jusqu'à ce qu'il atteigne une limite supérieure. Une fois la limite supérieure atteinte, le programme s'arrête.

**Variables:**

* R0 : Compteur
* R1 : Limite supérieure
* R2 : Limite inférieure

**Boucle principale:**

* La boucle principale se répète jusqu'à ce que le compteur atteigne la limite supérieure (R0 == R1).
* Si le compteur est inférieur à la limite inférieure (R0 < R2), le programme revient au début de la boucle.

**Affichage du compteur:**

* Le compteur est affiché sur l'écran à l'aide du sous-programme `AfficheCompteur`.
* Le sous-programme convertit le compteur en chaîne ASCII et l'affiche sur l'écran.

**Registres de sortie et d'entrée:**

* Les registres 0x80 (sortie) et 0x84 (entrée) sont utilisés pour communiquer avec l'écran.
* Les registres 0x00 et 0x01 sont utilisés respectivement pour les numéros de registres de sortie et d'entrée.
* Le mode d'adressage 0x02 (indirect) est utilisé pour accéder aux données dans les registres de sortie et d'entrée.

**Arrêt du programme:**

* Lorsque le compteur atteint la limite supérieure, le programme s'arrête avec l'instruction `HALT`.