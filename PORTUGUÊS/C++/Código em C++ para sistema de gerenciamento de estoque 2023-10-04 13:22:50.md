Claro! Vou gerar um código complexo em C++ para você. Este código é um exemplo de um sistema de gerenciamento de estoque para uma loja fictícia. Ele permite ao usuário adicionar, remover, atualizar e exibir informações sobre os produtos disponíveis na loja.

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Estrutura de dados para representar um produto
struct Produto {
    string nome;
    int quantidade;
    float preco;
};

// Função para adicionar um novo produto
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

// Função para remover um produto existente
void removerProduto(vector<Produto>& estoque) {
    string nomeProduto;
    cout << "Digite o nome do produto a ser removido: ";
    cin >> nomeProduto;

    for (auto it = estoque.begin(); it != estoque.end(); ++it) {
        if (it->nome == nomeProduto) {
            estoque.erase(it);
            cout << "Produto removido com sucesso!" << endl;
            return;
        }
    }

    cout << "Produto não encontrado!" << endl;
}

// Função para atualizar as informações de um produto
void atualizarProduto(vector<Produto>& estoque) {
    string nomeProduto;
    cout << "Digite o nome do produto a ser atualizado: ";
    cin >> nomeProduto;

    for (auto& produto : estoque) {
        if (produto.nome == nomeProduto) {
            cout << "Digite a nova quantidade do produto: ";
            cin >> produto.quantidade;
            cout << "Digite o novo preço do produto: ";
            cin >> produto.preco;

            cout << "Produto atualizado com sucesso!" << endl;
            return;
        }
    }

    cout << "Produto não encontrado!" << endl;
}

// Função para exibir todos os produtos do estoque
void exibirEstoque(const vector<Produto>& estoque) {
    cout << "Estoque:" << endl;
    for (const auto& produto : estoque) {
        cout << "Nome: " << produto.nome << endl;
        cout << "Quantidade: " << produto.quantidade << endl;
        cout << "Preço: R$ " << produto.preco << endl;
        cout << "-----------------------" << endl;
    }
}

int main() {
    vector<Produto> estoque; // Vetor para armazenar os produtos

    int opcao;
    do {
        cout << "==== Menu ====" << endl;
        cout << "1. Adicionar produto" << endl;
        cout << "2. Remover produto" << endl;
        cout << "3. Atualizar produto" << endl;
        cout << "4. Exibir estoque" << endl;
        cout << "0. Sair" << endl;
        cout << "Escolha uma opção: ";
        cin >> opcao;

        switch (opcao) {
            case 1:
                adicionarProduto(estoque);
                break;
            case 2:
                removerProduto(estoque);
                break;
            case 3:
                atualizarProduto(estoque);
                break;
            case 4:
                exibirEstoque(estoque);
                break;
            case 0:
                cout << "Encerrando o programa..." << endl;
                break;
            default:
                cout << "Opção inválida!" << endl;
                break;
        }
    } while (opcao != 0);

    return 0;
}
```

Este código utiliza a linguagem C++ para criar um sistema de gerenciamento de estoque. Ele utiliza uma estrutura de dados `struct` chamada `Produto` para representar as informações de cada produto, como nome, quantidade e preço.

O programa principal possui um menu interativo que permite ao usuário escolher diferentes opções, como adicionar um novo produto, remover um produto existente, atualizar as informações de um produto ou exibir o estoque completo. Todas essas funcionalidades são implementadas em funções separadas para facilitar a manutenção e a reutilização do código.

O estoque é armazenado em um vetor de `Produto`, permitindo que vários produtos sejam adicionados e gerenciados. O programa utiliza loops `for` para percorrer o estoque e encontrar produtos específicos com base em seu nome.

Espero que este código atenda às suas expectativas!