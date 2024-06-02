**Partie 1 : Déclaration des segments**

```assembly
.model small
.stack 1024
.data
texte:  db "Ceci est un texte complexe en Assembly.", 13, 10, '$'
```

**Partie 2 : Fonctions**

```assembly
; Affiche une chaîne de caractères
affiche_chaine proc
    push    bp      ; Sauvegarder le pointeur de base
    mov     bp, sp  ; Pointe le pointeur de base sur la pile
    push    si      ; Sauvegarder le registre SI
    mov     si, [bp + 4]  ; Récupérer l'adresse de la chaîne
    call    affiche_car    ; Appeler la fonction d'affichage caractère par caractère
    pop     si      ; Restaurer le registre SI
    pop     bp      ; Restaurer le pointeur de base
    ret     4       ; Libérer l'espace de la chaîne sur la pile
affiche_chaine endp

; Affiche un caractère
affiche_car proc
    push    bp      ; Sauvegarder le pointeur de base
    mov     bp, sp  ; Pointe le pointeur de base sur la pile
    push    di      ; Sauvegarder le registre DI
    mov     di, [bp + 4]  ; Récupérer le caractère à afficher
    mov     ah, 02h   ; Code INT pour l'affichage caractère
    int     21h     ; Interruption pour afficher le caractère
    pop     di      ; Restaurer le registre DI
    pop     bp      ; Restaurer le pointeur de base
    ret     4       ; Libérer l'espace du caractère sur la pile
affiche_car endp

; Compte le nombre de mots dans une chaîne de caractères
compte_mots proc
    push    bp      ; Sauvegarder le pointeur de base
    mov     bp, sp  ; Pointe le pointeur de base sur la pile
    push    si      ; Sauvegarder le registre SI
    push    ax      ; Sauvegarder le registre AX
    mov     si, [bp + 4]  ; Récupérer l'adresse de la chaîne
    mov     ax, 0    ; Initialiser le compteur de mots
    call    compter_mots_loop
    pop     ax      ; Restaurer le registre AX
    pop     si      ; Restaurer le registre SI
    pop     bp      ; Restaurer le pointeur de base
    ret     4       ; Libérer l'espace de la chaîne sur la pile
compte_mots endp

; Boucle pour compter les mots dans une chaîne de caractères
compter_mots_loop proc
    cmp     [si], '$'  ; Vérifier si le caractère actuel est un terminateur de chaîne
    je      fin_compter_mots
    cmp     [si], ' '   ; Vérifier si le caractère actuel est un espace
    je      incrémenter_compteur
    inc     si       ; Passer au caractère suivant
    jmp     compter_mots_loop
incrémenter_compteur:
    inc     ax       ; Incrémenter le compteur de mots
    cmp     [si], '$'  ; Vérifier si le caractère suivant est un terminateur de chaîne
    je      fin_compter_mots
    inc     si       ; Passer au caractère suivant
    jmp     compter_mots_loop
fin_compter_mots:
    ret     4       ; Libérer l'espace de la chaîne sur la pile
compter_mots_loop endp
```

**Partie 3 : Programme principal**

```assembly
.code
main proc
    push    bp      ; Sauvegarder le registre BP
    mov     bp, sp  ; Pointe BP sur la pile
    push    offset texte  ; Adresse de la chaîne de caractères
    call    affiche_chaine  ; Affichage de la chaîne

    push    offset texte  ; Adresse de la chaîne de caractères
    call    compte_mots    ; Compte le nombre de mots dans la chaîne
    mov     ax, [bp + 4]  ; Récupère le nombre de mots
    add     sp, 2       ; Libère l'espace de la valeur retournée
    push    ax         ; Affiche le nombre de mots à l'écran
    call    affiche_car    ; Affiche chaque chiffre du nombre
    add     sp, 2       ; Libère l'espace du nombre sur la pile
    mov     ah, 4Ch   ; Code INT pour quitter le programme
    int     21h     ; Interruption pour quitter le programme
main endp
```

**Explication du code :**

Ce code est complexe car il implémente plusieurs fonctions et utilise des techniques d'assemblage avancées.

* **Fonctions :**
    * `affiche_chaine` : Affiche une chaîne de caractères en utilisant l'interruption INT 21h.
    * `affiche_car` : Affiche un caractère à l'écran.
    * `compte_mots` : Compte le nombre de mots dans une chaîne de caractères.

* **Programme principal :**
    * Affiche la chaîne de caractères `texte`.
    * Compte le nombre de mots dans la chaîne.
    * Affiche le nombre de mots à l'écran.
    * Termine le programme en utilisant l'interruption INT 21h avec le code 4Ch.