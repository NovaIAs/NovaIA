Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

struct Pessoa {
    std::string nome;
    int idade;
};

bool compararIdade(const Pessoa& p1, const Pessoa& p2) {
    return p1.idade < p2.idade;
}

int main() {
    std::vector<Pessoa> pessoas;

    // Preenche o vetor com algumas pessoas
    pessoas.push_back({"Alice", 25});
    pessoas.push_back({"João", 35});
    pessoas.push_back({"Maria", 20});
    pessoas.push_back({"Pedro", 30});
    pessoas.push_back({"Ana", 28});

    // Ordena as pessoas por idade
    std::sort(pessoas.begin(), pessoas.end(), compararIdade);

    // Imprime as pessoas ordenadas
    std::cout << "Pessoas ordenadas por idade:" << std::endl;
    for (const auto& pessoa : pessoas) {
        std::cout << pessoa.nome << ", " << pessoa.idade << " anos" << std::endl;
    }

    return 0;
}
```

Neste código, temos uma estrutura chamada `Pessoa`, que armazena o nome e a idade de uma pessoa. Em seguida, temos uma função `compararIdade` que compara duas pessoas com base em suas idades.

No `main()`, criamos um vetor `pessoas` e preenchemos com algumas pessoas. Em seguida, usamos a função `std::sort` para ordenar o vetor `pessoas` com base na idade, utilizando a função de comparação `compararIdade`.

Por fim, imprimimos as pessoas ordenadas por idade no console.

Espero que este código atenda às suas expectativas!