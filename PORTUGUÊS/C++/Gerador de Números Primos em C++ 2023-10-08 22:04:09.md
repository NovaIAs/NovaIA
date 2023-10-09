#include <iostream>
#include <vector>

// Função para verificar se um número é primo
bool ehPrimo(int numero) {
    // Um número é primo se ele for maior que 1 e não for divisível por nenhum número, exceto 1 e ele mesmo
    if (numero <= 1) {
        return false;
    }
    for (int i = 2; i < numero; i++) {
        if (numero % i == 0) {
            return false;
        }
    }
    return true;
}

// Função para gerar os números primos em um intervalo
std::vector<int> gerarPrimos(int inicio, int fim) {
    std::vector<int> primos;
    for (int numero = inicio; numero <= fim; numero++) {
        if (ehPrimo(numero)) {
            primos.push_back(numero);
        }
    }
    return primos;
}

// Função principal
int main() {
    int inicio, fim;

    // Solicita ao usuário o intervalo desejado
    std::cout << "Digite o valor inicial do intervalo: ";
    std::cin >> inicio;
    std::cout << "Digite o valor final do intervalo: ";
    std::cin >> fim;

    // Gera os números primos no intervalo informado
    std::vector<int> primos = gerarPrimos(inicio, fim);

    // Exibe os números primos gerados
    std::cout << "Números primos encontrados no intervalo [" << inicio << ", " << fim << "]:\n";
    for (int primo : primos) {
        std::cout << primo << " ";
    }
    std::cout << std::endl;

    return 0;
}

Neste código em C++, implementei um programa que recebe um intervalo de números do usuário e gera todos os números primos contidos nesse intervalo. 

A função "ehPrimo" é responsável por verificar se um número é primo. Ela recebe um número como parâmetro e retorna verdadeiro se ele for primo ou falso caso contrário. A função utiliza um loop para verificar se o número é divisível por algum outro número, exceto 1 e ele mesmo. Se for divisível, o número não é primo.

A função "gerarPrimos" recebe o valor inicial e final do intervalo e retorna um vetor contendo todos os números primos encontrados nesse intervalo. Ela percorre todos os números do intervalo e utiliza a função "ehPrimo" para verificar se cada número é primo. Caso seja, o número é adicionado ao vetor.

Na função principal (main), o programa solicita ao usuário o intervalo desejado e chama a função "gerarPrimos" para gerar os números primos. Por fim, os números primos encontrados são exibidos na tela.