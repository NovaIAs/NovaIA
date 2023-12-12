Claro! Aqui está um código complexo em C++ que utiliza classes, herança e polimorfismo para criar um sistema de gerenciamento de funcionários em uma empresa:

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

class Funcionario {
protected:
    string nome;
    int idade;
    double salario;
public:
    Funcionario(string _nome, int _idade, double _salario) : nome(_nome), idade(_idade), salario(_salario) {}
    virtual void mostrarDados() = 0;
    virtual double calcularSalario() = 0;
};

class Gerente : public Funcionario {
private:
    string departamento;
public:
    Gerente(string _nome, int _idade, double _salario, string _departamento) : Funcionario(_nome, _idade, _salario), departamento(_departamento) {}
    void mostrarDados() override {
        cout << "--- Dados do Gerente ---" << endl;
        cout << "Nome: " << nome << endl;
        cout << "Idade: " << idade << endl;
        cout << "Salário: R$" << salario << endl;
        cout << "Departamento: " << departamento << endl;
    }
    double calcularSalario() override {
        return salario * 1.2;
    }
};

class Desenvolvedor : public Funcionario {
private:
    string linguagem;
public:
    Desenvolvedor(string _nome, int _idade, double _salario, string _linguagem) : Funcionario(_nome, _idade, _salario), linguagem(_linguagem) {}
    void mostrarDados() override {
        cout << "--- Dados do Desenvolvedor ---" << endl;
        cout << "Nome: " << nome << endl;
        cout << "Idade: " << idade << endl;
        cout << "Salário: R$" << salario << endl;
        cout << "Linguagem: " << linguagem << endl;
    }
    double calcularSalario() override {
        return salario * 1.1;
    }
};

int main() {
    vector<Funcionario*> funcionarios;

    funcionarios.push_back(new Gerente("João", 35, 5000, "Vendas"));
    funcionarios.push_back(new Desenvolvedor("Maria", 28, 4000, "C++"));
    funcionarios.push_back(new Gerente("Pedro", 42, 6000, "Marketing"));

    for (Funcionario* funcionario : funcionarios) {
        funcionario->mostrarDados();
        cout << "Salário total: R$" << funcionario->calcularSalario() << endl << endl;
    }

    for (Funcionario* funcionario : funcionarios) {
        delete funcionario;
    }
    funcionarios.clear();

    return 0;
}
```

Este código cria uma hierarquia de classes para representar diferentes tipos de funcionários em uma empresa: Gerente e Desenvolvedor. A classe base `Funcionario` é abstrata e define duas funções virtuais puras, `mostrarDados()` e `calcularSalario()`, que devem ser implementadas nas classes derivadas.

As classes derivadas `Gerente` e `Desenvolvedor` herdam da classe `Funcionario` e implementam suas próprias versões das funções virtuais puras. A função `mostrarDados()` exibe os dados específicos de cada tipo de funcionário, enquanto a função `calcularSalario()` retorna o salário total considerando um bônus específico de cada tipo.

No `main()`, um vetor de ponteiros para `Funcionario` é criado e preenchido com objetos das classes derivadas. Em seguida, um loop percorre o vetor e chama as funções `mostrarDados()` e `calcularSalario()` para cada funcionário, exibindo os resultados na tela.

Por fim, todos os objetos são liberados da memória e o vetor é limpo.

Espero que este código complexo em C++ tenha atendido às suas expectativas!