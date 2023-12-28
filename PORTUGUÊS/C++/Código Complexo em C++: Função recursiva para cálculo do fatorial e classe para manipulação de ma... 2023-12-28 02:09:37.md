Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <cmath>

// Função recursiva para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * fatorial(n - 1);
    }
}

// Classe para manipulação de matrizes
class Matriz {
    private:
        int** data;
        int linhas;
        int colunas;
    
    public:
        // Construtor da classe
        Matriz(int nLinhas, int nColunas) {
            linhas = nLinhas;
            colunas = nColunas;
            
            // Aloca dinamicamente a matriz
            data = new int*[linhas];
            for (int i = 0; i < linhas; i++) {
                data[i] = new int[colunas];
            }
        }
        
        // Destrutor da classe
        ~Matriz() {
            // Libera a memória alocada para a matriz
            for (int i = 0; i < linhas; i++) {
                delete[] data[i];
            }
            delete[] data;
        }
        
        // Função para preencher a matriz com valores
        void preencher() {
            for (int i = 0; i < linhas; i++) {
                for (int j = 0; j < colunas; j++) {
                    data[i][j] = i + j;
                }
            }
        }
        
        // Função para imprimir a matriz
        void imprimir() {
            for (int i = 0; i < linhas; i++) {
                for (int j = 0; j < colunas; j++) {
                    std::cout << data[i][j] << " ";
                }
                std::cout << std::endl;
            }
        }
};

int main() {
    // Exemplo de uso da função fatorial
    int numero = 5;
    int resultado = fatorial(numero);
    std::cout << "O fatorial de " << numero << " é " << resultado << std::endl;
    
    // Exemplo de uso da classe Matriz
    int nLinhas = 3;
    int nColunas = 3;
    Matriz matriz(nLinhas, nColunas);
    matriz.preencher();
    std::cout << "Matriz preenchida:" << std::endl;
    matriz.imprimir();
    
    return 0;
}
```

Neste código em C++, foram exemplificados dois conceitos: a função recursiva para calcular o fatorial de um número e uma classe para manipulação de matrizes.

A função `fatorial` é uma função recursiva que recebe um número `n` e retorna o fatorial desse número. Ela utiliza a recursividade para calcular o fatorial, onde o caso base é quando `n` é igual a zero, e o fatorial de zero é definido como 1. Caso contrário, a função chama a si mesma passando `n - 1` como parâmetro e multiplica o resultado pelo próprio `n`.

A classe `Matriz` é uma classe que representa uma matriz de números inteiros. Ela possui um construtor que recebe o número de linhas e colunas da matriz, e aloca dinamicamente a memória para armazenar os elementos da matriz. O destrutor libera a memória alocada quando a classe é destruída. A classe também possui duas funções: `preencher`, que preenche a matriz com valores incrementais a partir de zero, e `imprimir`, que imprime os elementos da matriz.

No `main`, é mostrado um exemplo de uso da função `fatorial` e da classe `Matriz`. O fatorial de um número é calculado e exibido na tela, e uma matriz é criada, preenchida e impressa.

Espero que este código complexo atenda às suas expectativas! Caso precise de alguma modificação ou esclarecimento adicional, estou à disposição.