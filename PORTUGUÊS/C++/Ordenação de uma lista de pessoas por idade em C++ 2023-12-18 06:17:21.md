#include <iostream>
#include <vector>

using namespace std;

// Definição da classe Pessoa
class Pessoa {
public:
    string nome;
    int idade;
    
    Pessoa(string n, int i) {
        nome = n;
        idade = i;
    }
    
    void imprimir() {
        cout << "Nome: " << nome << endl;
        cout << "Idade: " << idade << endl;
    }
};

// Função para ordenar a lista de pessoas por idade
bool compararIdade(const Pessoa& p1, const Pessoa& p2) {
    return p1.idade < p2.idade;
}

int main() {
    // Criação de uma lista de pessoas
    vector<Pessoa> listaPessoas;
    
    // Adicionando algumas pessoas à lista
    listaPessoas.push_back(Pessoa("João", 30));
    listaPessoas.push_back(Pessoa("Maria", 25));
    listaPessoas.push_back(Pessoa("Pedro", 35));
    
    // Ordenando a lista de pessoas por idade
    sort(listaPessoas.begin(), listaPessoas.end(), compararIdade);
    
    // Imprimindo a lista de pessoas ordenada
    for (const auto& pessoa : listaPessoas) {
        pessoa.imprimir();
        cout << endl;
    }
    
    return 0;
}

/*****************************************************
 * Explicação do código                               *
 *****************************************************
 
Neste código em C++, temos a definição de uma classe chamada "Pessoa". Essa classe possui dois atributos, "nome" e "idade", e um construtor que recebe esses valores e os atribui aos atributos correspondentes da classe.

Também temos um método chamado "imprimir" que imprime o nome e a idade da pessoa na saída padrão.

Na função principal "main", criamos um vetor chamado "listaPessoas" do tipo "Pessoa" e adicionamos algumas pessoas a ele. Em seguida, utilizamos a função "sort" da biblioteca padrão do C++ para ordenar a lista de pessoas por idade, utilizando a função "compararIdade" como critério de ordenação.

A função "compararIdade" é uma função auxiliar que recebe duas pessoas como parâmetro e retorna um valor booleano indicando se a idade da primeira pessoa é menor que a idade da segunda pessoa. Essa função é utilizada internamente pela função "sort" para realizar a ordenação.

Por fim, percorremos a lista de pessoas ordenada e chamamos o método "imprimir" de cada pessoa, para que seus dados sejam exibidos na saída padrão.

Este código é um exemplo simples de como criar uma classe, utilizar um vetor e ordená-lo em C++. Ele pode ser adaptado e utilizado como base para implementações mais complexas e específicas.