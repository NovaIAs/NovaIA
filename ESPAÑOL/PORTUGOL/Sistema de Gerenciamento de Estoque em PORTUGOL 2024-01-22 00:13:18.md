```portugol

// Este código é um exemplo de um sistema de gerenciamento de estoque em PORTUGOL.

#include <stdio.h>
#include <stdlib.h>

// Define a estrutura de um produto.
typedef struct {
    int código;
    char nome[50];
    float preço;
    int quantidade;
} Produto;

// Define a estrutura de um nó da lista ligada.
typedef struct {
    Produto produto;
    struct Nó *próximo;
} Nó;

// Define a estrutura da lista ligada.
typedef struct {
    Nó *primeiro;
    Nó *último;
    int tamanho;
} ListaLigada;

// Cria uma lista ligada vazia.
ListaLigada criaListaLigada() {
    ListaLigada lista;
    lista.primeiro = NULL;
    lista.último = NULL;
    lista.tamanho = 0;
    return lista;
}

// Insere um produto no final da lista ligada.
void insereProduto(ListaLigada *lista, Produto produto) {
    Nó *novoNó = malloc(sizeof(Nó));
    novoNó->produto = produto;
    novoNó->próximo = NULL;
    if (lista->primeiro == NULL) {
        lista->primeiro = novoNó;
        lista->último = novoNó;
    } else {
        lista->último->próximo = novoNó;
        lista->último = novoNó;
    }
    lista->tamanho++;
}

// Remove um produto da lista ligada pelo código.
void removeProduto(ListaLigada *lista, int código) {
    Nó *atual = lista->primeiro;
    Nó *anterior = NULL;
    while (atual != NULL && atual->produto.código != código) {
        anterior = atual;
        atual = atual->próximo;
    }
    if (atual == NULL) {
        return;
    }
    if (anterior == NULL) {
        lista->primeiro = atual->próximo;
    } else {
        anterior->próximo = atual->próximo;
    }
    if (atual == lista->último) {
        lista->último = anterior;
    }
    free(atual);
    lista->tamanho--;
}

// Busca um produto na lista ligada pelo código.
Produto *buscaProduto(ListaLigada *lista, int código) {
    Nó *atual = lista->primeiro;
    while (atual != NULL && atual->produto.código != código) {
        atual = atual->próximo;
    }
    if (atual == NULL) {
        return NULL;
    }
    return &atual->produto;
}

// Imprime a lista ligada de produtos.
void imprimeListaLigada(ListaLigada lista) {
    Nó *atual = lista.primeiro;
    while (atual != NULL) {
        printf("%d %s %.2f %d\n", atual->produto.código, atual->produto.nome, atual->produto.preço, atual->produto.quantidade);
        atual = atual->próximo;
    }
}

// Lê um produto do usuário.
Produto lêProduto() {
    Produto produto;
    printf("Código: ");
    scanf("%d", &produto.código);
    printf("Nome: ");
    scanf("%s", produto.nome);
    printf("Preço: ");
    scanf("%f", &produto.preço);
    printf("Quantidade: ");
    scanf("%d", &produto.quantidade);
    return produto;
}

// Menu principal do programa.
int menu() {
    int opção;
    printf("1 - Inserir produto\n");
    printf("2 - Remover produto\n");
    printf("3 - Buscar produto\n");
    printf("4 - Imprimir lista de produtos\n");
    printf("5 - Sair\n");
    printf("Opção: ");
    scanf("%d", &opção);
    return opção;
}

// Programa principal.
int main() {
    ListaLigada lista = criaListaLigada();
    int opção;
    do {
        opção = menu();
        switch (opção) {
            case 1:
                insereProduto(&lista, lêProduto());
                break;
            case 2:
                printf("Código do produto a ser removido: ");
                int código;
                scanf("%d", &código);
                removeProduto(&lista, código);
                break;
            case 3:
                printf("Código do produto a ser buscado: ");
                int código2;
                scanf("%d", &código2);
                Produto *produto = buscaProduto(&lista, código2);
                if (produto == NULL) {
                    printf("Produto não encontrado.\n");
                } else {
                    printf("Código: %d\n", produto->código);
                    printf("Nome: %s\n", produto->nome);
                    printf("Preço: %.2f\n", produto->preço);
                    printf("Quantidade: %d\n", produto->quantidade);
                }
                break;
            case 4:
                imprimeListaLigada(lista);
                break;
            case 5:
                printf("Saindo do programa.\n");
                break;
            default:
                printf("Opção inválida.\n");
                break;
        }
    } while (opção != 5);
    return 0;
}

```

Este código é um exemplo de um sistema de gerenciamento de estoque em PORTUGOL. O sistema permite inserir, remover, buscar e imprimir produtos de um estoque.

O código é dividido em várias funções, cada uma com uma responsabilidade específica. A função `criaListaLigada()` cria uma lista ligada vazia, a função `insereProduto()` insere um produto no final da lista ligada, a função `removeProduto()` remove um produto da lista ligada pelo código, a função `buscaProduto()` busca um produto na lista ligada pelo código, a função `imprimeListaLigada()` imprime a lista ligada de produtos, a função `lêProduto()` lê um produto do usuário e a função `menu()` exibe o menu principal do programa.

O programa principal `main()` chama a função `menu()` para exibir o menu principal e, em seguida, chama as funções apropriadas dependendo da opção escolhida pelo usuário.