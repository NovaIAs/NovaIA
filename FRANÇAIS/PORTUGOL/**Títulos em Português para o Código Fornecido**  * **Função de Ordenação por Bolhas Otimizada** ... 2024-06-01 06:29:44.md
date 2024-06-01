**Fonction de tri à bulles optimisée**

```portuguol
funcao bolha-otim(lista:vetor de inteiro):vetor de inteiro
    tamanho := tamanho-vetor(lista)
    trocou := verdadeiro
    enquanto trocou = verdadeiro fazer
        trocou := falso
        para i de 1 ate tamanho - 1 fazer
            se lista[i] > lista[i+1] entao
                troca(lista[i], lista[i+1])
                trocou := verdadeiro
            fimse
        fimpara
        tamanho--
    fimenquanto
    retorne lista
fimfuncao
```

**Explicação:**

Cette fonction implémente le tri à bulles, un algorithme de tri simple mais inefficace. Il parcourt la liste d'éléments à trier à plusieurs reprises, échangeant les éléments adjacents qui ne sont pas dans l'ordre.

L'optimisation apportée dans cette version est la diminution progressive de la taille de la liste à trier après chaque passage. En effet, les éléments qui sont déjà triés à la fin de la liste n'ont plus besoin d'être comparés.

Le drapeau `trocou` indique si au moins un échange a eu lieu pendant un passage. S'il reste faux après un passage complet, cela signifie que la liste est triée et la fonction peut se terminer.

**Fonction de calcul de la factorielle**

```portuguol
funcao fatorial(n:inteiro):inteiro
    se n < 0 entao
        retorne -1
    senao
        se n = 0 ou n = 1 entao
            retorne 1
        senao
            retorne n * fatorial(n - 1)
        fimse
    fimse
fimfuncao
```

**Explication:**

Cette fonction calcule la factorielle d'un nombre entier. La factorielle d'un nombre est le produit de tous les nombres entiers positifs jusqu'à ce nombre.

La fonction gère les cas particuliers où `n` est négatif (renvoie -1) ou nul/un (renvoie 1). Pour les autres valeurs de `n`, elle appelle récursivement elle-même avec `n - 1` et multiplie le résultat par `n`.

**Fonction de recherche binaire**

```portuguol
funcao busca-binaria(lista:vetor de inteiro, valor:inteiro):inteiro
    esquerda := 1
    direita := tamanho-vetor(lista)
    enquanto esquerda <= direita fazer
        meio := (esquerda + direita) div 2
        se lista[meio] = valor entao
            retorne meio
        senao se valor < lista[meio] entao
            direita := meio - 1
        senao
            esquerda := meio + 1
        fimse
    fimenquanto
    retorne -1
fimfuncao
```

**Explication:**

Cette fonction effectue une recherche binaire dans une liste triée. Elle divise à plusieurs reprises la liste en deux moitiés jusqu'à ce qu'elle trouve le `valeur` ou détermine qu'il n'est pas présent dans la liste.

La fonction utilise des pointeurs `esquerda` et `droite` pour définir les limites de la partie actuelle de la liste à rechercher. Elle calcule la position médiane `meio` et compare `valeur` à l'élément de la liste à cette position.

**Fonction de création d'une pile**

```portuguol
tipo pilha
    topo:inteiro
    capacidade:inteiro
    itens:vetor[1:capacidade] de inteiro
fimtipo

funcao cria-pilha(capacidade:inteiro):pilha
    nova-pilha := cria(pilha)
    nova-pilha.topo := 0
    nova-pilha.capacidade := capacidade
    cria-vetor(nova-pilha.itens, capacidade)
    retorne nova-pilha
fimfuncao
```

**Explication:**

Cette fonction crée une structure de données appelée `pile`. Une pile est un type de structure de données qui suit le principe du "premier entré, premier sorti" (FIFO).

La fonction alloue de la mémoire pour la pile et initialise les champs `topo` et `capacité`. Elle crée également le tableau `itens` qui stockera les éléments de la pile.

**Fonction d'insertion dans une pile**

```portuguol
procedimento empilhar(pilha:pilha, valor:inteiro)
    se pilha.topo < pilha.capacidade - 1 entao
        pilha.topo++
        pilha.itens[pilha.topo] := valor
    senao
        chama overflow-pilha(pilha)
    fimse
fimprocedimento
```

**Explication:**

Cette procédure insère un nouvel élément dans une pile. Elle vérifie d'abord s'il y a suffisamment d'espace dans la pile, puis incrémente le champ `topo` et stocke la `valeur` dans le tableau `itens`.

Si la pile est pleine, cette procédure appelle une procédure distincte pour gérer le dépassement de capacité.

**Fonction d'extraction d'une pile**

```portuguol
funcao desempilhar(pilha:pilha):inteiro
    se pilha.topo > 0 entao
        valor := pilha.itens[pilha.topo]
        pilha.topo--
        retorne valor
    senao
        chama underflow-pilha(pilha)
        retorne -1
    fimse
fimfuncao
```

**Explication:**

Cette fonction supprime et renvoie le dernier élément ajouté à une pile. Elle vérifie d'abord s'il y a des éléments dans la pile, puis décrémente le champ `topo` et renvoie la `valeur` du tableau `itens`.

Si la pile est vide, cette fonction appelle une procédure distincte pour gérer le sous-dépassement de capacité.