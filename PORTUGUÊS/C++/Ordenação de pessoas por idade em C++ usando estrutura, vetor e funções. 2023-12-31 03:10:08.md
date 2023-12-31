Claro! Aqui está um exemplo de código em C++ que utiliza conceitos avançados e é bastante complexo:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Estrutura para representar uma pessoa
struct Pessoa {
    string nome;
    int idade;
};

// Função para comparar pessoas por idade (para ser usada na ordenação)
bool compararPorIdade(const Pessoa& pessoa1, const Pessoa& pessoa2) {
    return pessoa1.idade < pessoa2.idade;
}

// Função para imprimir uma pessoa
void imprimirPessoa(const Pessoa& pessoa) {
    cout << "Nome: " << pessoa.nome << endl;
    cout << "Idade: " << pessoa.idade << endl;
    cout << endl;
}

int main() {
    // Vetor de pessoas
    vector<Pessoa> pessoas;

    // Preencher o vetor com algumas pessoas
    pessoas.push_back({"João", 25});
    pessoas.push_back({"Maria", 30});
    pessoas.push_back({"Pedro", 20});
    pessoas.push_back({"Ana", 35});

    // Ordenar as pessoas por idade
    sort(pessoas.begin(), pessoas.end(), compararPorIdade);

    // Imprimir as pessoas ordenadas
    cout << "Pessoas ordenadas por idade:" << endl;
    for (const Pessoa& pessoa : pessoas) {
        imprimirPessoa(pessoa);
    }

    return 0;
}
```

Neste código, temos uma estrutura `Pessoa` que representa uma pessoa com nome e idade. Em seguida, temos uma função `compararPorIdade` que é usada para ordenar o vetor de pessoas por idade. A função `imprimirPessoa` é responsável por imprimir os dados de uma pessoa.

No `main()`, criamos um vetor de pessoas e preenchemos com alguns dados. Em seguida, utilizamos a função `sort` para ordenar as pessoas por idade, utilizando a função `compararPorIdade` como critério de ordenação. Por fim, imprimimos as pessoas ordenadas.

Este código é apenas um exemplo de um programa complexo em C++ que utiliza estruturas de dados, funções e algoritmos da biblioteca padrão. É importante ressaltar que a complexidade de um código não está necessariamente relacionada ao seu tamanho, mas sim à sua estrutura e aos conceitos utilizados.