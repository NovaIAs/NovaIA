**Code PORTUGOL complexe**

```portugal
#include <stdio.h>

struct NoABB {
    int valor;
    struct NoABB *esquerda;
    struct NoABB *direita;
};

typedef struct NoABB NoABB;

NoABB *criarNoABB(int valor) {
    NoABB *novoNo = malloc(sizeof(NoABB));
    novoNo->valor = valor;
    novoNo->esquerda = NULL;
    novoNo->direita = NULL;
    return novoNo;
}

NoABB *inserirNoABB(NoABB *raiz, int valor) {
    if (raiz == NULL) {
        return criarNoABB(valor);
    } else if (valor < raiz->valor) {
        raiz->esquerda = inserirNoABB(raiz->esquerda, valor);
    } else if (valor > raiz->valor) {
        raiz->direita = inserirNoABB(raiz->direita, valor);
    }
    return raiz;
}

void imprimirABB(NoABB *raiz) {
    if (raiz != NULL) {
        imprimirABB(raiz->esquerda);
        printf("%d ", raiz->valor);
        imprimirABB(raiz->direita);
    }
}

int main() {
    // Criar uma árvore binária de busca (ABB)
    NoABB *raiz = NULL;
    raiz = inserirNoABB(raiz, 50);
    raiz = inserirNoABB(raiz, 30);
    raiz = inserirNoABB(raiz, 70);
    raiz = inserirNoABB(raiz, 20);
    raiz = inserirNoABB(raiz, 40);
    raiz = inserirNoABB(raiz, 60);
    raiz = inserirNoABB(raiz, 80);

    // Imprimir a ABB em ordem crescente
    printf("ABB em ordem crescente: ");
    imprimirABB(raiz);

    return 0;
}
```

**Explication du code**

Ce code PORTUGOL implémente une structure de données appelée arbre binaire de recherche (ABB). Une ABB est un arbre binaire ordonné, où les valeurs dans le sous-arbre gauche de chaque nœud sont inférieures à la valeur du nœud, et les valeurs dans le sous-arbre droit sont supérieures à la valeur du nœud.

La structure `NoABB` représente un nœud de l'ABB. Chaque nœud a trois champs : `valor`, `esquerda` et `direita`. Le champ `valor` stocke la valeur du nœud, et les champs `esquerda` et `direita` pointent vers les sous-arbres gauche et droit du nœud, respectivement.

La fonction `criarNoABB` crée un nouveau nœud avec la valeur donnée.

La fonction `inserirNoABB` insère un nouveau nœud dans l'ABB. Elle compare la valeur du nouveau nœud à la valeur du nœud racine. Si la valeur du nouveau nœud est inférieure, elle est insérée dans le sous-arbre gauche. Si la valeur du nouveau nœud est supérieure, elle est insérée dans le sous-arbre droit.

La fonction `imprimirABB` imprime l'ABB en ordre croissant. Elle utilise la traversée en profondeur dans l'ordre (inOrder) pour visiter les nœuds de l'ABB et les imprimer dans l'ordre croissant.

Dans la fonction `main`, une ABB est créée et initialisée avec quelques valeurs. Ensuite, la fonction `imprimirABB` est appelée pour imprimer l'ABB en ordre croissant.