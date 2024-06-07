**Programme : Tri de Bulles**

Ce programme trie un tableau d'entiers en utilisant l'algorithme de tri à bulles.

**Code :**

```assembly
.data
tableau:       .int 5, 2, 1, 3, 4

.text
.global main
main:
    mov esi, tableau  ; pointeur sur le début du tableau

boucle_ext:
    cmp esi, tableau + taille_tableau * 4  ; fin du tableau ?
    je fini              ; oui, fin du programme

boucle_int:
    cmp esi + 4, tableau + taille_tableau * 4  ; fin du tableau ?
    je boucle_ext          ; oui, passer à la boucle extérieure

    mov eax, [esi]       ; charger le premier élément
    mov ebx, [esi + 4]   ; charger le deuxième élément
    cmp eax, ebx         ; comparer les deux éléments
    jle continuer         ; si le premier élément est plus petit ou égal, continuer

    mov [esi], ebx       ; échanger les deux éléments
    mov [esi + 4], eax
continuer:
    add esi, 4           ; passer à l'élément suivant

    jmp boucle_int       ; recommencer la boucle intérieure

fini:
    ret

taille_tableau:    .equ 5
```

**Explication :**

* **Variables de données :**
    * `tableau` : tableau d'entiers à trier

* **Fonctions :**
    * `main` : fonction principale qui effectue le tri

* **Instructions :**
    * `mov` : charge une valeur dans un registre
    * `cmp` : compare deux valeurs
    * `je` : saute à une étiquette si les deux valeurs sont égales
    * `jle` : saute à une étiquette si la première valeur est plus petite ou égale à la deuxième
    * `add` : ajoute une valeur à un registre
    * `ret` : retourne de la fonction

* **Étiquettes :**
    * `boucle_ext` : étiquette pour la boucle extérieure
    * `boucle_int` : étiquette pour la boucle intérieure
    * `fini` : étiquette pour la fin du programme

**Fonctionnement :**

1. Le programme initialise le pointeur `esi` sur le début du tableau.
2. La boucle extérieure (étiquette `boucle_ext`) parcourt le tableau jusqu'à la fin.
3. La boucle intérieure (étiquette `boucle_int`) compare chaque paire d'éléments adjacents dans le tableau.
4. Si un élément est plus grand que l'élément suivant, les deux éléments sont échangés.
5. L'ensemble du processus est répété jusqu'à ce que le tableau soit entièrement trié.
6. Lorsque le tableau est trié, le programme saute à l'étiquette `fini` et se termine.