#include <iostream>
#include <cmath>

// Classe para representar um número complexo
class Complexo {
private:
    float real;
    float imaginario;

public:
    // Construtor que recebe os valores para parte real e imaginária
    Complexo(float r, float i) : real(r), imaginario(i) {}

    // Função para calcular o módulo do número complexo
    float modulo() {
        return sqrt(real * real + imaginario * imaginario);
    }

    // Função para calcular a conjugada do número complexo
    Complexo conjugado() {
        return Complexo(real, -imaginario);
    }

    // Sobrecarga do operador de adição
    Complexo operator+(const Complexo& outro) {
        return Complexo(real + outro.real, imaginario + outro.imaginario);
    }

    // Sobrecarga do operador de subtração
    Complexo operator-(const Complexo& outro) {
        return Complexo(real - outro.real, imaginario - outro.imaginario);
    }

    // Sobrecarga do operador de multiplicação
    Complexo operator*(const Complexo& outro) {
        float r = real * outro.real - imaginario * outro.imaginario;
        float i = real * outro.imaginario + imaginario * outro.real;
        return Complexo(r, i);
    }

    // Sobrecarga do operador de divisão
    Complexo operator/(const Complexo& outro) {
        float denominador = outro.real * outro.real + outro.imaginario * outro.imaginario;
        float r = (real * outro.real + imaginario * outro.imaginario) / denominador;
        float i = (imaginario * outro.real - real * outro.imaginario) / denominador;
        return Complexo(r, i);
    }

    // Função para imprimir o número complexo
    void imprimir() {
        std::cout << real << " + " << imaginario << "i" << std::endl;
    }
};

int main() {
    // Criando dois números complexos
    Complexo c1(3, 4);
    Complexo c2(2, -1);

    // Calculando o módulo e imprimindo
    std::cout << "Modulo de c1: " << c1.modulo() << std::endl;
    std::cout << "Modulo de c2: " << c2.modulo() << std::endl;

    // Calculando a conjugada e imprimindo
    Complexo conjugadaC1 = c1.conjugado();
    std::cout << "Conjugada de c1: ";
    conjugadaC1.imprimir();

    Complexo conjugadaC2 = c2.conjugado();
    std::cout << "Conjugada de c2: ";
    conjugadaC2.imprimir();

    // Realizando operações com os números complexos e imprimindo
    Complexo soma = c1 + c2;
    std::cout << "Soma: ";
    soma.imprimir();

    Complexo subtracao = c1 - c2;
    std::cout << "Subtracao: ";
    subtracao.imprimir();

    Complexo multiplicacao = c1 * c2;
    std::cout << "Multiplicacao: ";
    multiplicacao.imprimir();

    Complexo divisao = c1 / c2;
    std::cout << "Divisao: ";
    divisao.imprimir();

    return 0;
}