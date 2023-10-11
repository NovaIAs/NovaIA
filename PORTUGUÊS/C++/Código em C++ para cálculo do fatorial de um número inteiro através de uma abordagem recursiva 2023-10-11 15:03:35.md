Claro! Vou criar um código complexo em C++ para você. Este código é um exemplo de um programa simples que realiza o cálculo do fatorial de um número inteiro, utilizando uma abordagem recursiva.

```cpp
#include <iostream>
using namespace std;

// Função recursiva para calcular o fatorial
int fatorial(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * fatorial(n - 1);
    }
}

int main() {
    int numero;

    cout << "Digite um número inteiro: ";
    cin >> numero;

    // Verifica se o número é negativo
    if (numero < 0) {
        cout << "Erro: O fatorial não está definido para números negativos!" << endl;
    } else {
        int resultado = fatorial(numero);
        cout << "O fatorial de " << numero << " é " << resultado << endl;
    }

    return 0;
}
```

Neste código, temos uma função chamada `fatorial`, que é implementada de forma recursiva. Ela recebe um número `n` como parâmetro e retorna o fatorial desse número.

Dentro da função `fatorial`, temos uma condição de parada: se `n` for igual a 0, isso significa que chegamos ao caso base, e o resultado retornado será 1.

Caso contrário, chamamos a função `fatorial` novamente, passando como argumento `n - 1`, e multiplicamos o valor de `n` pelo resultado dessa chamada recursiva.

No `main`, solicitamos ao usuário que digite um número inteiro. Em seguida, verificamos se o número é negativo. Caso seja, exibimos uma mensagem de erro, pois o fatorial não está definido para números negativos.

Caso o número seja não-negativo, chamamos a função `fatorial` passando esse número como argumento e armazenamos o resultado em uma variável chamada `resultado`. Por fim, exibimos o resultado na tela.

Esse é apenas um exemplo de código complexo em C++. Existem muitas outras possibilidades e aplicações que podem ser desenvolvidas na linguagem.