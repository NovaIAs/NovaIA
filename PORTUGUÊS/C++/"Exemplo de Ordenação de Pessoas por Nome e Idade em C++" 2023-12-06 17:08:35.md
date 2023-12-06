Claro! Aqui está um exemplo de código em C++ que envolve múltiplas funcionalidades e algoritmos:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

struct Pessoa {
    std::string nome;
    int idade;
};

bool compararPorNome(const Pessoa& pessoa1, const Pessoa& pessoa2) {
    return pessoa1.nome < pessoa2.nome;
}

bool compararPorIdade(const Pessoa& pessoa1, const Pessoa& pessoa2) {
    return pessoa1.idade < pessoa2.idade;
}

int main() {
    std::vector<Pessoa> pessoas;

    // Preenchendo o vetor com algumas pessoas
    pessoas.push_back({"João", 25});
    pessoas.push_back({"Maria", 32});
    pessoas.push_back({"Ana", 19});
    pessoas.push_back({"Pedro", 41});
    pessoas.push_back({"Paula", 27});

    // Ordenando as pessoas por nome
    std::sort(pessoas.begin(), pessoas.end(), compararPorNome);

    std::cout << "Pessoas ordenadas por nome:" << std::endl;
    for(const auto& pessoa : pessoas) {
        std::cout << pessoa.nome << " - " << pessoa.idade << " anos" << std::endl;
    }
    std::cout << std::endl;

    // Ordenando as pessoas por idade
    std::sort(pessoas.begin(), pessoas.end(), compararPorIdade);

    std::cout << "Pessoas ordenadas por idade:" << std::endl;
    for(const auto& pessoa : pessoas) {
        std::cout << pessoa.nome << " - " << pessoa.idade << " anos" << std::endl;
    }
    std::cout << std::endl;

    return 0;
}
```

Este código demonstra o uso de structs, funções, vetores e algoritmos de ordenação em C++. Ele cria uma estrutura chamada "Pessoa", que possui um nome e uma idade. Em seguida, define duas funções adicionais para comparar pessoas por nome ou idade.

No `main()`, criamos um vetor de pessoas e preenchemos com algumas informações. Em seguida, usamos a função `std::sort` para ordenar as pessoas primeiro por nome e depois por idade. Finalmente, exibimos o resultado ordenado na saída.

Espero que este código atenda às suas expectativas!