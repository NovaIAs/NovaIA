Claro! Aqui está um exemplo de um código em C++ que realiza uma simulação de um sistema de gerenciamento de estoque de uma loja. O código está em português e possui comentários explicativos para facilitar a compreensão:

```cpp
#include <iostream>
#include <map>
#include <string>

using namespace std;

// Classe que representa um produto
class Produto {
public:
    string nome;
    int quantidade;

    Produto(string nome, int quantidade) {
        this->nome = nome;
        this->quantidade = quantidade;
    }
};

// Classe que representa o sistema de gerenciamento de estoque
class GerenciadorEstoque {
private:
    map<string, Produto> estoque;

public:
    // Método para adicionar um produto ao estoque
    void adicionarProduto(string nome, int quantidade) {
        if (estoque.find(nome) == estoque.end()) {
            Produto novoProduto(nome, quantidade);
            estoque.insert(make_pair(nome, novoProduto));
        } else {
            estoque[nome].quantidade += quantidade;
        }
    }

    // Método para remover um produto do estoque
    void removerProduto(string nome, int quantidade) {
        if (estoque.find(nome) != estoque.end()) {
            if (estoque[nome].quantidade >= quantidade) {
                estoque[nome].quantidade -= quantidade;
                if (estoque[nome].quantidade == 0) {
                    estoque.erase(nome);
                }
            } else {
                cout << "Não há quantidade suficiente de " << nome << " no estoque." << endl;
            }
        } else {
            cout << "O produto " << nome << " não está no estoque." << endl;
        }
    }

    // Método para exibir o estoque
    void exibirEstoque() {
        cout << "Estoque:" << endl;
        for (const auto& par : estoque) {
            cout << "Produto: " << par.second.nome << ", Quantidade: " << par.second.quantidade << endl;
        }
    }
};

int main() {
    GerenciadorEstoque gerenciador;

    gerenciador.adicionarProduto("Camiseta", 10);
    gerenciador.adicionarProduto("Calça", 5);
    gerenciador.adicionarProduto("Tênis", 15);

    gerenciador.exibirEstoque();

    gerenciador.removerProduto("Camiseta", 3);
    gerenciador.removerProduto("Calça", 10);
    gerenciador.removerProduto("Meia", 2);

    gerenciador.exibirEstoque();

    return 0;
}
```

Neste código, temos duas classes principais: `Produto` e `GerenciadorEstoque`. A classe `Produto` representa um produto com seu nome e quantidade em estoque. A classe `GerenciadorEstoque` é responsável por adicionar, remover e exibir os produtos no estoque.

No `main()`, é criado um objeto `GerenciadorEstoque` e são adicionados alguns produtos ao estoque. Em seguida, o estoque é exibido. Depois, alguns produtos são removidos do estoque e o estoque é exibido novamente.

Espero que este código atenda às suas expectativas!