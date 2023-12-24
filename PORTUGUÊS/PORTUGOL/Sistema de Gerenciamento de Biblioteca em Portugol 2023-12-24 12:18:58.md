Claro! Vou gerar um código complexo em Portugol para você. Este código é um exemplo de um sistema de gerenciamento de biblioteca, que permite adicionar, atualizar, remover e buscar livros. Ele utiliza estruturas de dados como vetores e matrizes, além de funções para realizar as operações.

```
// Definição das constantes
const int MAX_LIVROS = 100;
const int MAX_AUTORES = 50;
const int MAX_TITULO = 100;

// Definição da estrutura de dados para representar um livro
struct Livro {
    int codigo;
    char titulo[MAX_TITULO];
    char autor[MAX_AUTORES];
};

// Declaração do vetor de livros
Livro biblioteca[MAX_LIVROS];

// Declaração da variável de controle do número de livros na biblioteca
int numLivros = 0;

// Função para adicionar um livro na biblioteca
void adicionarLivro() {
    if (numLivros >= MAX_LIVROS) {
        printf("A biblioteca está cheia!\n");
        return;
    }

    Livro novoLivro;

    printf("Digite o código do livro: ");
    scanf("%d", &novoLivro.codigo);

    printf("Digite o título do livro: ");
    scanf(" %[^\n]s", novoLivro.titulo);

    printf("Digite o autor do livro: ");
    scanf(" %[^\n]s", novoLivro.autor);

    biblioteca[numLivros] = novoLivro;
    numLivros++;

    printf("Livro adicionado com sucesso!\n");
}

// Função para atualizar as informações de um livro na biblioteca
void atualizarLivro() {
    int codigo;

    printf("Digite o código do livro que deseja atualizar: ");
    scanf("%d", &codigo);

    int posicao = -1;

    for (int i = 0; i < numLivros; i++) {
        if (biblioteca[i].codigo == codigo) {
            posicao = i;
            break;
        }
    }

    if (posicao == -1) {
        printf("Livro não encontrado!\n");
        return;
    }

    printf("Digite o novo título do livro: ");
    scanf(" %[^\n]s", biblioteca[posicao].titulo);

    printf("Digite o novo autor do livro: ");
    scanf(" %[^\n]s", biblioteca[posicao].autor);

    printf("Livro atualizado com sucesso!\n");
}

// Função para remover um livro da biblioteca
void removerLivro() {
    int codigo;

    printf("Digite o código do livro que deseja remover: ");
    scanf("%d", &codigo);

    int posicao = -1;

    for (int i = 0; i < numLivros; i++) {
        if (biblioteca[i].codigo == codigo) {
            posicao = i;
            break;
        }
    }

    if (posicao == -1) {
        printf("Livro não encontrado!\n");
        return;
    }

    for (int i = posicao; i < numLivros - 1; i++) {
        biblioteca[i] = biblioteca[i + 1];
    }

    numLivros--;

    printf("Livro removido com sucesso!\n");
}

// Função para buscar um livro na biblioteca
void buscarLivro() {
    int codigo;

    printf("Digite o código do livro que deseja buscar: ");
    scanf("%d", &codigo);

    int posicao = -1;

    for (int i = 0; i < numLivros; i++) {
        if (biblioteca[i].codigo == codigo) {
            posicao = i;
            break;
        }
    }

    if (posicao == -1) {
        printf("Livro não encontrado!\n");
        return;
    }

    Livro livro = biblioteca[posicao];

    printf("Código: %d\n", livro.codigo);
    printf("Título: %s\n", livro.titulo);
    printf("Autor: %s\n", livro.autor);
}

// Função para exibir todos os livros da biblioteca
void listarLivros() {
    if (numLivros == 0) {
        printf("A biblioteca está vazia!\n");
        return;
    }

    for (int i = 0; i < numLivros; i++) {
        Livro livro = biblioteca[i];

        printf("Código: %d\n", livro.codigo);
        printf("Título: %s\n", livro.titulo);
        printf("Autor: %s\n", livro.autor);
        printf("--------------------\n");
    }
}

// Função principal
int main() {
    int opcao;

    do {
        printf("===== MENU =====\n");
        printf("1. Adicionar livro\n");
        printf("2. Atualizar livro\n");
        printf("3. Remover livro\n");
        printf("4. Buscar livro\n");
        printf("5. Listar livros\n");
        printf("0. Sair\n");
        printf("Digite a opção desejada: ");
        scanf("%d", &opcao);

        switch (opcao) {
            case 1:
                adicionarLivro();
                break;
            case 2:
                atualizarLivro();
                break;
            case 3:
                removerLivro();
                break;
            case 4:
                buscarLivro();
                break;
            case 5:
                listarLivros();
                break;
            case 0:
                printf("Saindo...\n");
                break;
            default:
                printf("Opção inválida!\n");
                break;
        }

        printf("--------------------\n");
    } while (opcao != 0);

    return 0;
}
```

Este código é um exemplo de um sistema de gerenciamento de biblioteca em Portugol. Ele permite adicionar, atualizar, remover e buscar livros na biblioteca. A estrutura de dados utilizada é um vetor de estruturas do tipo Livro. O programa apresenta um menu com opções para cada funcionalidade e realiza as operações de acordo com a escolha do usuário.

Espero que este código atenda às suas expectativas! Caso precise de mais algum código ou tenha alguma dúvida, estou à disposição.