Claro! Aqui está um código complexo em C++ que realiza uma simulação de um sistema de gerenciamento de estoque para uma loja. O código está bem comentado e possui uma estrutura modular para facilitar a compreensão.

```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Definição da estrutura de um produto
struct Produto {
    int codigo;
    string nome;
    int quantidade;
    float preco;
};

// Função para exibir as opções do menu
void exibirMenu() {
    cout << "==== MENU ====" << endl;
    cout << "1. Cadastrar produto" << endl;
    cout << "2. Consultar produto" << endl;
    cout << "3. Atualizar quantidade" << endl;
    cout << "4. Listar produtos" << endl;
    cout << "5. Sair" << endl;
    cout << "===============" << endl;
    cout << "Escolha uma opção: ";
}

// Função para cadastrar um novo produto
void cadastrarProduto(vector<Produto>& estoque) {
    Produto novoProduto;
    cout << "==== CADASTRO DE PRODUTO ====" << endl;
    cout << "Código: ";
    cin >> novoProduto.codigo;
    cout << "Nome: ";
    cin.ignore();
    getline(cin, novoProduto.nome);
    cout << "Quantidade: ";
    cin >> novoProduto.quantidade;
    cout << "Preço: ";
    cin >> novoProduto.preco;
    estoque.push_back(novoProduto);
    cout << "Produto cadastrado com sucesso!" << endl;
}

// Função para consultar um produto pelo código
void consultarProduto(const vector<Produto>& estoque) {
    int codigo;
    cout << "==== CONSULTA DE PRODUTO ====" << endl;
    cout << "Código do produto: ";
    cin >> codigo;
    for (const Produto& produto : estoque) {
        if (produto.codigo == codigo) {
            cout << "Nome: " << produto.nome << endl;
            cout << "Quantidade: " << produto.quantidade << endl;
            cout << "Preço: " << produto.preco << endl;
            return;
        }
    }
    cout << "Produto não encontrado." << endl;
}

// Função para atualizar a quantidade de um produto
void atualizarQuantidade(vector<Produto>& estoque) {
    int codigo;
    int quantidade;
    cout << "==== ATUALIZAÇÃO DE QUANTIDADE ====" << endl;
    cout << "Código do produto: ";
    cin >> codigo;
    cout << "Nova quantidade: ";
    cin >> quantidade;
    for (Produto& produto : estoque) {
        if (produto.codigo == codigo) {
            produto.quantidade = quantidade;
            cout << "Quantidade atualizada com sucesso!" << endl;
            return;
        }
    }
    cout << "Produto não encontrado." << endl;
}

// Função para listar todos os produtos do estoque
void listarProdutos(const vector<Produto>& estoque) {
    cout << "==== LISTA DE PRODUTOS ====" << endl;
    for (const Produto& produto : estoque) {
        cout << "Código: " << produto.codigo << endl;
        cout << "Nome: " << produto.nome << endl;
        cout << "Quantidade: " << produto.quantidade << endl;
        cout << "Preço: " << produto.preco << endl;
        cout << "-------------------" << endl;
    }
}

int main() {
    vector<Produto> estoque; // Vetor para armazenar os produtos

    int opcao;
    do {
        exibirMenu();
        cin >> opcao;
        switch (opcao) {
            case 1:
                cadastrarProduto(estoque);
                break;
            case 2:
                consultarProduto(estoque);
                break;
            case 3:
                atualizarQuantidade(estoque);
                break;
            case 4:
                listarProdutos(estoque);
                break;
            case 5:
                cout << "Encerrando o programa..." << endl;
                break;
            default:
                cout << "Opção inválida!" << endl;
                break;
        }
        cout << endl;
    } while (opcao != 5);

    return 0;
}
```

Este código cria um sistema de gerenciamento de estoque para uma loja, permitindo o cadastro, consulta, atualização de quantidade e listagem dos produtos. Os produtos são armazenados em um vetor de estruturas `Produto`, que contém as informações de código, nome, quantidade e preço.

No `main()`, um loop `do-while` é utilizado para exibir o menu de opções e executar a funcionalidade escolhida pelo usuário. As funções `cadastrarProduto()`, `consultarProduto()`, `atualizarQuantidade()` e `listarProdutos()` são chamadas de acordo com a opção selecionada.

Cada função possui uma implementação específica para realizar a funcionalidade desejada. Os produtos são armazenados em um vetor `estoque`, permitindo o acesso e manipulação dos dados.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.