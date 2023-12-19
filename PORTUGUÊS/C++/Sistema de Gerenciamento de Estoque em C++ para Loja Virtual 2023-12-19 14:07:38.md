Claro! Aqui está um código complexo em C++, criado especialmente para você. Este código é um exemplo de um sistema de gerenciamento de estoque para uma loja virtual.

```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Definição da estrutura de dados para representar um produto
struct Produto {
    string nome;
    int quantidade;
    float preco;
};

// Função para adicionar um novo produto ao estoque
void adicionarProduto(vector<Produto>& estoque) {
    Produto novoProduto;
    
    cout << "Nome do produto: ";
    cin.ignore();
    getline(cin, novoProduto.nome);
    
    cout << "Quantidade: ";
    cin >> novoProduto.quantidade;
    
    cout << "Preço: ";
    cin >> novoProduto.preco;
    
    estoque.push_back(novoProduto);
}

// Função para remover um produto do estoque
void removerProduto(vector<Produto>& estoque, string nomeProduto) {
    int indiceProduto = -1;
    
    for (int i = 0; i < estoque.size(); i++) {
        if (estoque[i].nome == nomeProduto) {
            indiceProduto = i;
            break;
        }
    }
    
    if (indiceProduto != -1) {
        estoque.erase(estoque.begin() + indiceProduto);
        cout << "Produto removido com sucesso!" << endl;
    } else {
        cout << "Produto não encontrado no estoque." << endl;
    }
}

// Função para exibir todos os produtos do estoque
void exibirEstoque(vector<Produto>& estoque) {
    cout << "===== ESTOQUE =====" << endl;
    
    for (int i = 0; i < estoque.size(); i++) {
        cout << "Produto: " << estoque[i].nome << endl;
        cout << "Quantidade: " << estoque[i].quantidade << endl;
        cout << "Preço: R$" << estoque[i].preco << endl;
        cout << "------------------------" << endl;
    }
    
    cout << "Total de produtos: " << estoque.size() << endl;
    cout << "====================" << endl;
}

int main() {
    vector<Produto> estoque;
    int opcao;
    
    do {
        cout << "===== MENU =====" << endl;
        cout << "1. Adicionar produto" << endl;
        cout << "2. Remover produto" << endl;
        cout << "3. Exibir estoque" << endl;
        cout << "0. Sair" << endl;
        cout << "================" << endl;
        cout << "Escolha uma opção: ";
        cin >> opcao;
        
        switch (opcao) {
            case 1:
                adicionarProduto(estoque);
                break;
            case 2:
                string nomeProduto;
                cout << "Nome do produto a ser removido: ";
                cin.ignore();
                getline(cin, nomeProduto);
                removerProduto(estoque, nomeProduto);
                break;
            case 3:
                exibirEstoque(estoque);
                break;
            case 0:
                cout << "Saindo do programa..." << endl;
                break;
            default:
                cout << "Opção inválida!" << endl;
                break;
        }
        
        cout << endl;
    } while (opcao != 0);
    
    return 0;
}
```

Este código em C++ cria um sistema de gerenciamento de estoque para uma loja virtual. O programa permite ao usuário adicionar produtos ao estoque, remover produtos existentes e exibir todos os produtos disponíveis no estoque.

A estrutura de dados utilizada é um vetor de estruturas `Produto`. Cada produto é representado pela estrutura `Produto`, que possui campos para o nome do produto, quantidade disponível e preço.

No `main()`, o programa exibe um menu com as opções disponíveis: adicionar produto, remover produto, exibir estoque e sair do programa. Dependendo da opção escolhida pelo usuário, as funções correspondentes são chamadas.

A função `adicionarProduto()` solicita ao usuário que informe o nome, quantidade e preço do produto a ser adicionado, em seguida, cria um novo objeto do tipo `Produto` e o adiciona ao estoque.

A função `removerProduto()` solicita ao usuário que informe o nome do produto a ser removido, em seguida, busca pelo produto no estoque e o remove, caso seja encontrado.

A função `exibirEstoque()` percorre o vetor de produtos e exibe as informações de cada produto, incluindo nome, quantidade e preço.

O programa continua em execução até que o usuário escolha a opção de sair (opção 0).

Espero que este exemplo atenda às suas expectativas! Se você tiver mais alguma solicitação, estou à disposição para ajudar.