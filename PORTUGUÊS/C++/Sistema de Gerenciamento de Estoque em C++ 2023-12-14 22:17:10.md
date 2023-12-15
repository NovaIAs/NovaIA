Claro! Vou criar um código complexo em C++ para você. Este código é um exemplo de um sistema de gerenciamento de estoque para uma loja fictícia. Ele permite ao usuário adicionar produtos, exibir o estoque, buscar produtos, atualizar as quantidades e remover produtos. Vou explicar cada parte do código conforme vou escrevendo.

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Definindo a estrutura de um produto
struct Produto {
    string nome;
    int quantidade;
    float preco;
};

// Função para adicionar um novo produto ao estoque
void adicionarProduto(vector<Produto>& estoque) {
    Produto novoProduto;

    cout << "Digite o nome do produto: ";
    cin >> novoProduto.nome;

    cout << "Digite a quantidade do produto: ";
    cin >> novoProduto.quantidade;

    cout << "Digite o preço do produto: ";
    cin >> novoProduto.preco;

    estoque.push_back(novoProduto);

    cout << "Produto adicionado com sucesso!" << endl;
}

// Função para exibir o estoque completo
void exibirEstoque(const vector<Produto>& estoque) {
    if (estoque.empty()) {
        cout << "O estoque está vazio." << endl;
    } else {
        cout << "Estoque:" << endl;
        for (const auto& produto : estoque) {
            cout << "Nome: " << produto.nome << endl;
            cout << "Quantidade: " << produto.quantidade << endl;
            cout << "Preço: " << produto.preco << endl;
            cout << endl;
        }
    }
}

// Função para buscar um produto pelo nome
void buscarProduto(const vector<Produto>& estoque, const string& nomeProduto) {
    bool encontrado = false;

    for (const auto& produto : estoque) {
        if (produto.nome == nomeProduto) {
            cout << "Produto encontrado:" << endl;
            cout << "Nome: " << produto.nome << endl;
            cout << "Quantidade: " << produto.quantidade << endl;
            cout << "Preço: " << produto.preco << endl;

            encontrado = true;
            break;
        }
    }

    if (!encontrado) {
        cout << "Produto não encontrado." << endl;
    }
}

// Função para atualizar a quantidade de um produto
void atualizarQuantidade(vector<Produto>& estoque, const string& nomeProduto, int novaQuantidade) {
    bool atualizado = false;

    for (auto& produto : estoque) {
        if (produto.nome == nomeProduto) {
            produto.quantidade = novaQuantidade;
            atualizado = true;
            break;
        }
    }

    if (atualizado) {
        cout << "Quantidade atualizada com sucesso!" << endl;
    } else {
        cout << "Produto não encontrado." << endl;
    }
}

// Função para remover um produto do estoque
void removerProduto(vector<Produto>& estoque, const string& nomeProduto) {
    int posicao = -1;

    for (size_t i = 0; i < estoque.size(); ++i) {
        if (estoque[i].nome == nomeProduto) {
            posicao = i;
            break;
        }
    }

    if (posicao != -1) {
        estoque.erase(estoque.begin() + posicao);
        cout << "Produto removido com sucesso!" << endl;
    } else {
        cout << "Produto não encontrado." << endl;
    }
}

// Função principal
int main() {
    vector<Produto> estoque;
    int opcao;
    string nomeProduto;

    do {
        cout << "Sistema de Gerenciamento de Estoque" << endl;
        cout << "1 - Adicionar produto" << endl;
        cout << "2 - Exibir estoque" << endl;
        cout << "3 - Buscar produto" << endl;
        cout << "4 - Atualizar quantidade do produto" << endl;
        cout << "5 - Remover produto" << endl;
        cout << "0 - Sair" << endl;
        cout << "Escolha uma opção: ";
        cin >> opcao;

        switch (opcao) {
            case 1:
                adicionarProduto(estoque);
                break;
            case 2:
                exibirEstoque(estoque);
                break;
            case 3:
                cout << "Digite o nome do produto: ";
                cin >> nomeProduto;
                buscarProduto(estoque, nomeProduto);
                break;
            case 4:
                cout << "Digite o nome do produto: ";
                cin >> nomeProduto;
                int novaQuantidade;
                cout << "Digite a nova quantidade: ";
                cin >> novaQuantidade;
                atualizarQuantidade(estoque, nomeProduto, novaQuantidade);
                break;
            case 5:
                cout << "Digite o nome do produto: ";
                cin >> nomeProduto;
                removerProduto(estoque, nomeProduto);
                break;
            case 0:
                cout << "Saindo do programa..." << endl;
                break;
            default:
                cout << "Opção inválida. Tente novamente." << endl;
                break;
        }

        cout << endl;
    } while (opcao != 0);

    return 0;
}
```

Este código é bastante extenso e complexo, mas permite a criação de um sistema de gerenciamento de estoque. Ele utiliza uma estrutura de dados chamada `Produto`, que armazena o nome, a quantidade e o preço de cada produto. Utiliza também um `vector` para armazenar todos os produtos do estoque.

As funções `adicionarProduto`, `exibirEstoque`, `buscarProduto`, `atualizarQuantidade` e `removerProduto` realizam as operações necessárias para adicionar, exibir, buscar, atualizar e remover produtos do estoque.

Na função principal `main`, é apresentado um menu com opções para o usuário selecionar. Dependendo da opção escolhida, as funções correspondentes são chamadas para executar a operação desejada.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, estou à disposição para ajudar.