**Code Assembly en Français**

```assembly
.data
  message:      .asciz "Bienvenue à l'Assemblée !"

.text
.global main

main:
  mov     eax, 4                         ; syscall write
  mov     ebx, 1                         ; stdout
  mov     ecx, message                   ; adresse du message
  mov     edx, strlen(message)          ; longueur du message
  int     0x80                           ; appel système

  mov     eax, 1                         ; syscall exit
  mov     ebx, 0                         ; code de retour
  int     0x80                           ; appel système
```

**Explication du Code**

Ce code Assembly est conçu pour afficher le message "Bienvenue à l'Assemblée !" sur la sortie standard.

* **Section .data**
    * **message:** Contient le message à afficher.

* **Section .text**
    * **main:**
        * **mov eax, 4:** Déclenche l'appel système `write`.
        * **mov ebx, 1:** Spécifie la sortie standard (stdout).
        * **mov ecx, message:** Charge l'adresse du message.
        * **mov edx, strlen(message):** Charge la longueur du message.
        * **int 0x80:** Exécute l'appel système.
        * **mov eax, 1:** Déclenche l'appel système `exit`.
        * **mov ebx, 0:** Spécifie un code de retour de 0.
        * **int 0x80:** Exécute l'appel système.

Dans ce code, l'appel système `write` est utilisé pour écrire le message sur la sortie standard, et l'appel système `exit` est utilisé pour terminer le programme en renvoyant un code de retour de 0.