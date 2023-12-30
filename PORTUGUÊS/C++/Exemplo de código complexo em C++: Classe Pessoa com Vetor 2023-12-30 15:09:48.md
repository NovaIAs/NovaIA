Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Classe Pessoa
class Pessoa {
private:
    string nome;
    int idade;

public:
    Pessoa(string nome, int idade) {
        this->nome = nome;
        this->idade = idade;
    }

    void apresentar() {
        cout << "Nome: " << nome << endl;
        cout << "Idade: " << idade << endl;
    }
};

// Função principal
int main() {
    vector<Pessoa> pessoas;

    // Adicionando pessoas ao vetor
    pessoas.push_back(Pessoa("João", 20));
    pessoas.push_back(Pessoa("Maria", 25));
    pessoas.push_back(Pessoa("Pedro", 30));

    // Apresentando as pessoas
    for (int i = 0; i < pessoas.size(); i++) {
        cout << "Pessoa " << i+1 << ":" << endl;
        pessoas[i].apresentar();
        cout << endl;
    }

    return 0;
}
```

Neste exemplo, temos uma classe chamada "Pessoa" que possui os atributos "nome" e "idade". A classe tem um construtor que recebe esses valores e um método chamado "apresentar" que imprime na tela o nome e a idade da pessoa.

No código principal, criamos um vetor de objetos do tipo "Pessoa" chamado "pessoas". Em seguida, adicionamos três pessoas ao vetor usando o método push_back(). 

Depois, utilizamos um loop for para percorrer o vetor e imprimir as informações de cada pessoa, chamando o método "apresentar".

Esse código é uma simples demonstração de como usar classes e vetores em C++. É importante lembrar que a complexidade do código pode variar dependendo das funcionalidades e estruturas de dados utilizadas.