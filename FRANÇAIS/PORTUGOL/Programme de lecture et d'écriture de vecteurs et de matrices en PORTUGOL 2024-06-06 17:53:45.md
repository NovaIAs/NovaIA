**Code en PORTUGOL**

```
procedimento principal()
    inteiro vetor_a[10], vetor_b[10]
    real matriz_a[5,5], matriz_b[5,5]
    caractere texto

    ler(texto)
    escrever(texto, " foi lido")

    ler(vetor_a)
    repetir i = 1 até 10
        ler(vetor_b[i])
    fim repetir

    ler(matriz_a)
    repetir i = 1 até 5
        repetir j = 1 até 5
            ler(matriz_b[i,j])
        fim repetir
    fim repetir

    escreva("Vetor A: ")
    repetir i = 1 até 10
        escrever(vetor_a[i])
    fim repetir

    escreva("Vetor B: ")
    repetir i = 1 até 10
        escrever(vetor_b[i])
    fim repetir

    escreva("Matriz A: ")
    repetir i = 1 até 5
        repetir j = 1 até 5
            escrever(matriz_a[i,j])
        fim repetir
    fim repetir

    escreva("Matriz B: ")
    repetir i = 1 até 5
        repetir j = 1 até 5
            escrever(matriz_b[i,j])
        fim repetir
    fim repetir

fim procedimento
```

**Explication du code**

Ce code en PORTUGOL effectue les opérations suivantes :

* **Déclaration des variables** :
    * `vetor_a` et `vetor_b` sont des tableaux d'entiers de taille 10.
    * `matriz_a` et `matriz_b` sont des matrices réelles de taille 5x5.
    * `texto` est une variable de caractère.

* **Lecture des entrées** :
    * Le code lit une chaîne de caractères dans la variable `texto`.
    * Il lit ensuite les éléments du tableau `vetor_a`.
    * Enfin, il lit les éléments de la matrice `matriz_a`.

* **Écriture des sorties** :
    * Le code écrit la chaîne de caractères lue dans `texto`.
    * Il écrit ensuite les éléments des tableaux `vetor_a` et `vetor_b`.
    * Enfin, il écrit les éléments des matrices `matriz_a` et `matriz_b`.