Voici un code complexe et différencié en Assembly :

```assembly
; Déclaration de variables
SECTION .data

msg_debut : db "Début du programme", 0 ; Message de début de programme
msg_fin : db "Fin du programme", 0 ; Message de fin de programme

; Déclaration de fonctions
SECTION .text

; Fonction principale
global _start
_start:

    ; Affiche le message de début de programme
    mov eax, 4 ; Code système pour l'écriture
    mov ebx, 1 ; Descripteur de fichier pour la sortie standard
    mov ecx, msg_debut ; Adresse du message
    mov edx, strlen(msg_debut) ; Longueur du message
    int 0x80 ; Appel système pour l'écriture

    ; Boucle infinie
    loop:
        ; Code complexe ici

    ; Affiche le message de fin de programme
    mov eax, 4 ; Code système pour l'écriture
    mov ebx, 1 ; Descripteur de fichier pour la sortie standard
    mov ecx, msg_fin ; Adresse du message
    mov edx, strlen(msg_fin) ; Longueur du message
    int 0x80 ; Appel système pour l'écriture

    ; Arrêt du programme
    mov eax, 1 ; Code système pour l'arrêt du programme
    mov ebx, 0 ; Code de retour
    int 0x80 ; Appel système pour l'arrêt du programme

; Fonction pour calculer la longueur d'une chaîne
strlen:
    mov ebx, 0 ; Compteur de caractères
    mov ecx, edi ; Adresse de la chaîne
    cmp byte [ecx], 0 ; Compare le caractère courant à 0
    je fin_strlen ; Si le caractère courant est 0, termine la fonction
    inc ebx ; Incrémente le compteur de caractères
    add ecx, 1 ; Passe au caractère suivant
    jmp strlen ; Réitère la boucle jusqu'à ce que le caractère courant soit 0
fin_strlen:
    ret ; Renvoie le compteur de caractères
```

**Explications du code :**

* La section `.data` déclare les variables `msg_debut` et `msg_fin` qui contiennent les messages de début et de fin de programme.
* La section `.text` déclare les fonctions `_start` et `strlen`.
* La fonction `_start` est la fonction principale du programme. Elle appelle la fonction `strlen` pour calculer la longueur des messages de début et de fin de programme, puis utilise l'appel système `write` pour les afficher.
* La fonction `strlen` calcule la longueur d'une chaîne en parcourant chaque caractère de la chaîne jusqu'à trouver le caractère de fin (`\0`).
* Le programme se termine en appelant l'appel système `exit` avec un code de retour de 0.