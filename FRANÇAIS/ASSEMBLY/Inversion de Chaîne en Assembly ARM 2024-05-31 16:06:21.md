**Programme d'inversion d'une chaîne en Assembly ARM**

```
.global main
.text

main:
    push {lr}
    mov r0, #0  ; Index de départ de la chaîne
    mov r1, #10 ; Longueur de la chaîne
    bl inverse_chaine  ; Inversion de la chaîne
    ldr r0, =message ; Adresse du message
    bl affichage_message  ; Affichage de la chaîne inversée
    pop {pc}

inverse_chaine:
    push {r4-r5}  ; Sauvegarde des registres
    mov r4, r0  ; Index de départ de la chaîne
    mov r5, r1  ; Longueur de la chaîne
    sub r5, r5, #1  ; Index de fin de la chaîne
1:
    ldr r2, [r4]  ; Lecture d'un caractère de la chaîne
    ldr r3, [r5]  ; Lecture du caractère symétrique
    strb r3, [r4]  ; Écriture du caractère symétrique
    strb r2, [r5]  ; Écriture du caractère d'origine
    add r4, r4, #1  ; Incrémentation de l'index de départ
    sub r5, r5, #1  ; Décrémentation de l'index de fin
    cmp r4, r5  ; Comparaison des index
    ble 1b  ; Boucle tant que l'index de départ est inférieur ou égal à l'index de fin
    pop {r4-r5}  ; Restauration des registres
    bx lr  ; Retour à l'appelant

affichage_message:
    push {r0-r1}  ; Sauvegarde des registres
    mov r1, #1  ; Longueur maximale du message
1:
    ldrb r2, [r0]  ; Lecture d'un caractère du message
    cmp r2, #0  ; Fin du message ?
    beq 2f  ; Oui, sortie de la boucle
    mov r0, r0, lsl #1  ; Décalage de l'adresse du message
    add r0, r0, lsl #1  ; Ajout de 4 au décalage
    add r0, r2  ; Calcul de l'adresse du caractère
    ldr r2, [r0]  ; Lecture du caractère
    bl affichage_caractere  ; Affichage du caractère
    cmp r2, #13  ; Saut de ligne ?
    beq 3f  ; Oui, sortie de la boucle
    add r0, r0, #1  ; Incrémentation de l'adresse du message
    sub r1, r1, #1  ; Décrémentation de la longueur restante
    b 1b  ; Boucle tant qu'il reste des caractères et que le saut de ligne n'a pas été rencontré
2:
    bl affichage_caractere  ; Affichage du caractère de fin
3:
    pop {r0-r1}  ; Restauration des registres
    bx lr  ; Retour à l'appelant

affichage_caractere:
    push {r1}  ; Sauvegarde du registre
    mov r1, r0  ; Caractère à afficher
    bl affichage_byte  ; Affichage du caractère en tant que byte
    pop {r1}  ; Restauration du registre
    bx lr  ; Retour à l'appelant

affichage_byte:
    push {lr}  ; Sauvegarde du registre lien de retour
    mov r0, #0  ; Fichier de sortie (stdout)
    mov r1, r0  ; Caractère à afficher
    mov r2, #1  ; Nombre de caractères à écrire
    bl write  ; Écriture du caractère
    pop {pc}  ; Retour à l'appelant

.data
message: .asciz "Chaîne à inverser\n"  ; Message à afficher

```

**Explications du code :**

Ce code Assembly ARM inverse une chaîne de caractères et affiche le résultat.

* La fonction `main` :
    * Initialise l'index de départ et la longueur de la chaîne.
    * Appelle la fonction `inverse_chaine` pour inverser la chaîne.
    * Affiche la chaîne inversée à l'aide de la fonction `affichage_message`.

* La fonction `inverse_chaine` :
    * Inverse la chaîne en place en échangeant les caractères symétriques.
    * Utilise des registres pour suivre les index de départ et de fin de la chaîne.
    * Boucle jusqu'à ce que les index se croisent.

* La fonction `affichage_message` :
    * Affiche le message donné par l'adresse en mémoire `r0`.
    * Arrête l'affichage lorsque la fin du message est atteinte ('\0') ou lorsqu'un saut de ligne est rencontré ('\n').
    * Utilise la fonction `affichage_caractere` pour afficher chaque caractère.

* La fonction `affichage_caractere` :
    * Affiche le caractère donné en mémoire `r0`.
    * Utilise la fonction `affichage_byte` pour écrire le caractère sur la sortie standard.

* La fonction `affichage_byte` :
    * Écrit un byte sur la sortie standard using la fonction système `write`.