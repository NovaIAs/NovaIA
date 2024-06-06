```assembly
; ===== Section données =====
section .data

; Tableau des messages
messages:
        .asciz "Message 1\n"  ; Message 1
        .asciz "Message 2\n"  ; Message 2
        .asciz "Message 3\n"  ; Message 3

; Adresse de départ du tableau
addr_messages: .quad messages

; Taille du tableau
nb_messages: .quad 3

; ===== Section texte =====
section .text

; ===== Déclaration des fonctions =====
extern printf

; ===== Fonction principale =====
global main
main:
        ; Déclarer les variables
        mov rax, nb_messages
        mov rbx, addr_messages

        ; Boucle d'affichage des messages
        loop:
                ; Charger l'adresse du message courant
                mov rdx, rbx
                mov rdx, [rdx]

                ; Afficher le message
                mov rsi, rdx
                call printf

                ; Incrémenter l'adresse du message courant
                add rbx, 8

                ; Décrémenter le nombre de messages restants
                dec rax

                ; Tester si tous les messages ont été affichés
                jnz loop

        ; Retourner 0 pour indiquer la fin du programme
        mov rax, 0
        ret

```

**Explication du code :**

* **Section données** : contient les messages à afficher.
* **Section texte** : contient le code exécutable.
* **Fonction principale `main`** :
    * Déclare les variables nécessaires.
    * Boucle sur les messages :
        * Charge l'adresse du message courant.
        * Affiche le message.
        * Incrémente l'adresse du message courant.
        * Décrémente le nombre de messages restants.
        * Teste si tous les messages ont été affichés.
* La variable `nb_messages` contient le nombre de messages dans le tableau.
* La variable `addr_messages` contient l'adresse du tableau des messages.