#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Classe para representar uma pessoa
class Pessoa {
public:
    string nome;
    int idade;

    Pessoa(string nome, int idade) {
        this->nome = nome;
        this->idade = idade;
    }

    void apresentar() {
        cout << "Olá, meu nome é " << nome << " e tenho " << idade << " anos." << endl;
    }
};

// Classe para representar uma lista de pessoas
class ListaPessoas {
private:
    vector<Pessoa> pessoas;

public:
    void adicionarPessoa(Pessoa pessoa) {
        pessoas.push_back(pessoa);
    }

    void removerPessoa(string nome) {
        for (int i = 0; i < pessoas.size(); i++) {
            if (pessoas[i].nome == nome) {
                pessoas.erase(pessoas.begin() + i);
                break;
            }
        }
    }

    void apresentarPessoas() {
        for (int i = 0; i < pessoas.size(); i++) {
            pessoas[i].apresentar();
        }
    }
};

int main() {
    ListaPessoas lista;

    // Adicionando pessoas à lista
    Pessoa pessoa1("João", 25);
    Pessoa pessoa2("Maria", 30);
    Pessoa pessoa3("Carlos", 40);
    lista.adicionarPessoa(pessoa1);
    lista.adicionarPessoa(pessoa2);
    lista.adicionarPessoa(pessoa3);

    // Apresentando as pessoas da lista
    lista.apresentarPessoas();

    // Removendo a pessoa "Maria" da lista
    lista.removerPessoa("Maria");

    // Apresentando as pessoas da lista novamente
    lista.apresentarPessoas();

    return 0;
}