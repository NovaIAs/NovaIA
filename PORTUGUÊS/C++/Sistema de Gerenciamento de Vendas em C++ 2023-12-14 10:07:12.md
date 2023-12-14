Claro! Aqui está um exemplo de um código complexo em C++. Este código é um sistema de gerenciamento de vendas para uma loja fictícia. Ele permite cadastrar produtos, realizar vendas, exibir relatórios de vendas e gerenciar o estoque.

```cpp
#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct Produto {
    string nome;
    float preco;
    int quantidade;
};

vector<Produto> estoque;

void cadastrarProduto() {
    Produto novoProduto;
    
    cout << "Nome do produto: ";
    getline(cin, novoProduto.nome);
    
    cout << "Preco do produto: ";
    cin >> novoProduto.preco;
    
    cout << "Quantidade em estoque: ";
    cin >> novoProduto.quantidade;
    
    estoque.push_back(novoProduto);
    
    cout << "Produto cadastrado com sucesso!" << endl;
}

void realizarVenda() {
    string nomeProduto;
    int quantidade;
    
    cout << "Nome do produto: ";
    getline(cin, nomeProduto);
    
    cout << "Quantidade a ser vendida: ";
    cin >> quantidade;
    
    for (auto& produto : estoque) {
        if (produto.nome == nomeProduto) {
            if (produto.quantidade >= quantidade) {
                produto.quantidade -= quantidade;
                cout << "Venda realizada com sucesso!" << endl;
                return;
            } else {
                cout << "Quantidade insuficiente em estoque!" << endl;
                return;
            }
        }
    }
    
    cout << "Produto nao encontrado!" << endl;
}

void exibirRelatorio() {
    float totalVendas = 0.0;
    int totalProdutos = 0;
    
    cout << "RELATORIO DE VENDAS" << endl;
    cout << "-------------------" << endl;
    
    for (const auto& produto : estoque) {
        float valorVendas = produto.preco * (produto.quantidadeInicial - produto.quantidade);
        totalVendas += valorVendas;
        totalProdutos += produto.quantidade;
        
        cout << "Produto: " << produto.nome << endl;
        cout << "Quantidade vendida: " << produto.quantidadeInicial - produto.quantidade << endl;
        cout << "Valor total das vendas: R$ " << valorVendas << endl;
        cout << "-------------------" << endl;
    }
    
    cout << "RESUMO" << endl;
    cout << "Total de produtos vendidos: " << totalProdutos << endl;
    cout << "Valor total das vendas: R$ " << totalVendas << endl;
}

int main() {
    int opcao;
    
    do {
        cout << "MENU" << endl;
        cout << "----" << endl;
        cout << "1 - Cadastrar produto" << endl;
        cout << "2 - Realizar venda" << endl;
        cout << "3 - Exibir relatorio de vendas" << endl;
        cout << "0 - Sair" << endl;
        cout << "Escolha uma opcao: ";
        cin >> opcao;
        cin.ignore(); // Limpa o buffer do teclado
        
        switch (opcao) {
            case 1:
                cadastrarProduto();
                break;
            case 2:
                realizarVenda();
                break;
            case 3:
                exibirRelatorio();
                break;
            case 0:
                cout << "Saindo..." << endl;
                break;
            default:
                cout << "Opcao invalida!" << endl;
                break;
        }
        
        cout << endl;
    } while (opcao != 0);
    
    return 0;
}
```

Neste código, utilizamos a estrutura `Produto` para representar cada item do estoque, com os atributos `nome`, `preco` e `quantidade`. Em seguida, temos um `vector` chamado `estoque` que armazena todos os produtos cadastrados.

As principais funções do programa são:
- `cadastrarProduto()`: solicita ao usuário o nome, preço e quantidade de um produto e o adiciona ao estoque.
- `realizarVenda()`: pede ao usuário o nome do produto e a quantidade a ser vendida. O programa verifica se a quantidade em estoque é suficiente e, se sim, atualiza o estoque.
- `exibirRelatorio()`: percorre o estoque e exibe um relatório de vendas, mostrando o nome do produto, a quantidade vendida e o valor total das vendas para cada produto. No final, é exibido um resumo com o total de produtos vendidos e o valor total das vendas.

No `main()`, temos um loop que exibe um menu ao usuário e permite que ele escolha uma opção. O loop continua até que o usuário selecione a opção de sair (0).

Espero que este código atenda às suas expectativas!