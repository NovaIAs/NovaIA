**Code complexe en Portugol**

```portugal
inicio
    // Déclaration de variables
    inteiro somaTotal = 0
    real media = 0
    vetor[real] notas = novo vetor(5)

    // Saída de texto
    escreva("Entre com as notas dos alunos:")

    // Entrada de dados
    para i = 1 até 5 faça
        leia(notas[i])
    fim para

    // Cálculo da soma total das notas
    para i = 1 até 5 faça
        somaTotal += notas[i]
    fim para

    // Cálculo da média das notas
    media = somaTotal / 5

    // Saída de texto
    escreva("A soma total das notas é:", somaTotal)
    escreva("A média das notas é:", media)

    // Ordenação das notas em ordem crescente
    para i = 1 até 4 faça
        para j = i + 1 até 5 faça
            se notas[i] > notas[j] então
                real aux = notas[i]
                notas[i] = notas[j]
                notas[j] = aux
            fim se
        fim para
    fim para

    // Saída das notas ordenadas
    escreva("Notas ordenadas:")
    para i = 1 até 5 faça
        escreva(notas[i])
    fim para

    // Verificação se há notas abaixo da média
    boolean existeNotaBaixa = falso
    para i = 1 até 5 faça
        se notas[i] < media então
            existeNotaBaixa = verdadeiro
        fim se
    fim para

    // Saída de texto
    se existeNotaBaixa então
        escreva("Existem notas abaixo da média")
    senão
        escreva("Todas as notas estão acima da média")
    fim se

fim
```

**Explication du code**

Ce code Portugol complexe effectue les opérations suivantes :

1. Déclare des variables pour stocker la somme totale des notes, la moyenne et un vecteur pour stocker les notes des élèves.
2. Demande aux utilisateurs d’entrer les notes des élèves.
3. Calcule la somme totale et la moyenne des notes.
4. Trie les notes dans l’ordre croissant.
5. Affiche les notes triées.
6. Vérifie s’il y a des notes inférieures à la moyenne.
7. Affiche un message approprié en fonction de l’existence de notes inférieures à la moyenne.