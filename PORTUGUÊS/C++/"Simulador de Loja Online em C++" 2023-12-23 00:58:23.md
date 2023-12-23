Claro! Abaixo está um exemplo de um código complexo em C++, que utiliza vários conceitos e recursos da linguagem. O código é um simulador de uma loja online, onde os usuários podem cadastrar-se, fazer compras, adicionar itens ao carrinho e finalizar a compra. 

```c++
#include <iostream>
#include <vector>

using namespace std;

class Produto {
  public:
    string nome;
    float preco;
    int quantidade;

    Produto(string nome, float preco, int quantidade) {
        this->nome = nome;
        this->preco = preco;
        this->quantidade = quantidade;
    }
};

class Usuario {
  public:
    string nome;
    string email;
    vector<Produto> carrinho;

    Usuario(string nome, string email) {
        this->nome = nome;
        this->email = email;
    }

    void adicionarAoCarrinho(Produto produto) {
        carrinho.push_back(produto);
    }

    void finalizarCompra() {
        float total = 0;
        cout << "Produtos do carrinho:" << endl;
        for (Produto produto : carrinho) {
            cout << "- " << produto.nome << " - R$" << produto.preco << endl;
            total += produto.preco;
        }
        cout << "Total: R$" << total << endl;
        carrinho.clear();
    }
};

int main() {
    vector<Produto> estoque;
    estoque.push_back(Produto("Camiseta", 39.99, 10));
    estoque.push_back(Produto("Calça", 79.99, 5));
    estoque.push_back(Produto("Tênis", 149.99, 3));

    cout << "Bem-vindo à loja online!" << endl;

    cout << "Digite seu nome: ";
    string nome;
    getline(cin, nome);

    cout << "Digite seu e-mail: ";
    string email;
    getline(cin, email);

    Usuario usuario(nome, email);

    char opcao;
    do {
        cout << endl;
        cout << "Menu:" << endl;
        cout << "1. Adicionar item ao carrinho" << endl;
        cout << "2. Finalizar compra" << endl;
        cout << "3. Sair" << endl;
        cout << "Escolha uma opção: ";
        cin >> opcao;

        switch (opcao) {
            case '1':
                cout << endl;
                cout << "Produtos disponíveis:" << endl;
                for (Produto produto : estoque) {
                    cout << "- " << produto.nome << " - R$" << produto.preco << " - Quantidade: " << produto.quantidade << endl;
                }

                cout << endl;
                cout << "Digite o número do produto que deseja adicionar ao carrinho: ";
                int escolha;
                cin >> escolha;

                if (escolha >= 1 && escolha <= estoque.size()) {
                    Produto produtoEscolhido = estoque[escolha - 1];

                    if (produtoEscolhido.quantidade > 0) {
                        usuario.adicionarAoCarrinho(produtoEscolhido);
                        produtoEscolhido.quantidade--;
                        cout << "Produto adicionado ao carrinho!" << endl;
                    } else {
                        cout << "Produto esgotado!" << endl;
                    }
                } else {
                    cout << "Opção inválida!" << endl;
                }

                break;
            case '2':
                usuario.finalizarCompra();
                break;
            case '3':
                cout << "Saindo..." << endl;
                break;
            default:
                cout << "Opção inválida!" << endl;
                break;
        }
    } while (opcao != '3');

    return 0;
}
```

Neste código, utilizamos classes (`Produto` e `Usuario`) para representar os produtos disponíveis na loja e os usuários que interagem com o sistema. Temos também um vetor `estoque` para armazenar os produtos disponíveis.

O programa solicita o nome e email do usuário, em seguida, exibe um menu com opções para adicionar itens ao carrinho, finalizar a compra ou sair. O usuário pode escolher um produto para adicionar ao carrinho, desde que esteja disponível no estoque e possui quantidade suficiente. Ao finalizar a compra, o programa exibe os produtos selecionados e o valor total da compra.

Espero que esse código atenda às suas expectativas! Fique à vontade para fazer alterações ou adicionar mais funcionalidades se desejar.