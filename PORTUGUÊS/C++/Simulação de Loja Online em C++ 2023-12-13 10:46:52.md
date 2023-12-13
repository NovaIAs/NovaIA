Claro! Vou criar um código complexo em C++ para você. Este código simulará uma loja online onde será possível adicionar produtos ao carrinho, realizar o pagamento e calcular o valor total da compra. Vou explicar o código à medida que vou criando:

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Definição da classe Produto
class Produto {
private:
    string nome;
    float preco;

public:
    Produto(string _nome, float _preco) {
        nome = _nome;
        preco = _preco;
    }

    string getNome() {
        return nome;
    }

    float getPreco() {
        return preco;
    }
};

// Função para adicionar produtos ao carrinho
void adicionarProduto(vector<Produto>& carrinho, Produto produto) {
    carrinho.push_back(produto);
    cout << "Produto \"" << produto.getNome() << "\" adicionado ao carrinho." << endl;
}

// Função para remover produtos do carrinho
void removerProduto(vector<Produto>& carrinho, string nomeProduto) {
    for (int i = 0; i < carrinho.size(); i++) {
        if (carrinho[i].getNome() == nomeProduto) {
            carrinho.erase(carrinho.begin() + i);
            cout << "Produto \"" << nomeProduto << "\" removido do carrinho." << endl;
            return;
        }
    }
    cout << "Produto \"" << nomeProduto << "\" não encontrado no carrinho." << endl;
}

// Função para calcular o valor total da compra
float calcularTotal(vector<Produto>& carrinho) {
    float total = 0;
    for (int i = 0; i < carrinho.size(); i++) {
        total += carrinho[i].getPreco();
    }
    return total;
}

int main() {
    vector<Produto> carrinho;

    // Criando alguns produtos
    Produto p1("Camiseta", 29.99);
    Produto p2("Calça Jeans", 89.99);
    Produto p3("Tênis", 149.99);
    Produto p4("Boné", 19.99);

    // Adicionando produtos ao carrinho
    adicionarProduto(carrinho, p1);
    adicionarProduto(carrinho, p2);
    adicionarProduto(carrinho, p3);
    adicionarProduto(carrinho, p4);

    // Removendo um produto do carrinho
    removerProduto(carrinho, "Calça Jeans");

    // Calculando o valor total da compra
    float total = calcularTotal(carrinho);
    cout << "Valor total da compra: R$" << total << endl;

    return 0;
}
```

Neste código, começamos importando as bibliotecas necessárias: `iostream`, `vector` e `string`. Em seguida, declaramos a classe `Produto` com dois atributos: `nome` (do tipo `string`) e `preco` (do tipo `float`). Dentro da classe, temos o construtor que recebe o nome e o preço do produto e dois métodos para retornar o nome e o preço do produto.

Depois, definimos três funções: `adicionarProduto`, `removerProduto` e `calcularTotal`. A função `adicionarProduto` recebe uma referência para um vetor de produtos e um produto para ser adicionado ao carrinho. A função `removerProduto` recebe uma referência para um vetor de produtos e o nome do produto a ser removido do carrinho. A função `calcularTotal` recebe uma referência para um vetor de produtos e retorna o valor total da compra.

No `main`, criamos um vetor chamado `carrinho` para armazenar os produtos selecionados pelo usuário. Em seguida, criamos alguns produtos utilizando o construtor da classe `Produto` e adicionamos esses produtos ao carrinho utilizando a função `adicionarProduto`. Também removemos um produto do carrinho utilizando a função `removerProduto`.

Por fim, calculamos o valor total da compra utilizando a função `calcularTotal` e exibimos o resultado na tela.

Espero que este código atenda às suas expectativas! Lembre-se que ele é apenas um exemplo e pode ser adaptado ou modificado de acordo com suas necessidades.