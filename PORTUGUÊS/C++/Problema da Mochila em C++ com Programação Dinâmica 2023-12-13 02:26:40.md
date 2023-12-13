Claro! Aqui está um exemplo de código em C++ que utiliza estruturas de dados complexas e algoritmos avançados para resolver um problema de otimização.

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Definição da estrutura de dados para representar um item
struct Item {
    int peso;
    int valor;
};

// Função de comparação para ordenar os itens pelo valor/peso (maior primeiro)
bool compararItem(const Item& item1, const Item& item2) {
    return (item1.valor / item1.peso) > (item2.valor / item2.peso);
}

// Função para resolver o problema da Mochila utilizando programação dinâmica
int resolverMochila(int capacidade, vector<Item>& itens) {
    int n = itens.size();

    // Criar uma matriz para armazenar a tabela de valores máximos
    vector<vector<int>> tabela(n + 1, vector<int>(capacidade + 1));

    // Inicializar a primeira linha e a primeira coluna com zeros
    for (int i = 0; i <= n; i++) {
        tabela[i][0] = 0;
    }
    for (int j = 0; j <= capacidade; j++) {
        tabela[0][j] = 0;
    }

    // Preencher a tabela com os valores máximos
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= capacidade; j++) {
            if (itens[i - 1].peso <= j) {
                tabela[i][j] = max(tabela[i - 1][j], itens[i - 1].valor + tabela[i - 1][j - itens[i - 1].peso]);
            } else {
                tabela[i][j] = tabela[i - 1][j];
            }
        }
    }

    // Retornar o valor máximo obtido
    return tabela[n][capacidade];
}

int main() {
    int capacidade;
    cout << "Digite a capacidade da mochila: ";
    cin >> capacidade;

    int numItens;
    cout << "Digite o número de itens: ";
    cin >> numItens;

    vector<Item> itens(numItens);
    for (int i = 0; i < numItens; i++) {
        cout << "Digite o peso e o valor do item " << i + 1 << ": ";
        cin >> itens[i].peso >> itens[i].valor;
    }

    // Ordenar os itens pelo valor/peso (maior primeiro)
    sort(itens.begin(), itens.end(), compararItem);

    // Resolver o problema da mochila
    int valorMaximo = resolverMochila(capacidade, itens);

    cout << "O valor máximo que pode ser colocado na mochila é: " << valorMaximo << endl;

    return 0;
}
```

Neste código, é realizado um problema clássico de otimização conhecido como "Problema da Mochila". O objetivo é determinar o valor máximo que pode ser colocado em uma mochila com capacidade limitada, considerando que cada item possui um peso e um valor associado.

A solução utiliza programação dinâmica para preencher uma tabela com os valores máximos que podem ser obtidos para diferentes capacidades e números de itens. A função `resolverMochila` implementa o algoritmo principal, enquanto a função `compararItem` é utilizada para ordenar os itens pelo valor/peso.

No `main`, o usuário é solicitado a fornecer a capacidade da mochila e o número de itens. Em seguida, são solicitados os pesos e valores de cada item. Após isso, os itens são ordenados pelo valor/peso e a função `resolverMochila` é chamada para obter o valor máximo que pode ser colocado na mochila. O resultado é então exibido na tela.