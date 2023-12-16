Claro! Aqui está um código complexo em C++ que realiza uma série de operações matemáticas e manipulação de strings. O código é bastante extenso e possui comentários explicativos para facilitar o entendimento:

```cpp
#include <iostream>
#include <string>
#include <cmath>

// Função para verificar se um número é primo
bool ehPrimo(int numero) {
    if (numero <= 1) {
        return false;
    }

    for (int i = 2; i <= sqrt(numero); i++) {
        if (numero % i == 0) {
            return false;
        }
    }

    return true;
}

// Função para inverter uma string
std::string inverterString(std::string texto) {
    std::string invertida;

    for (int i = texto.length() - 1; i >= 0; i--) {
        invertida += texto[i];
    }

    return invertida;
}

int main() {
    int numero = 123456789;

    // Verifica se o número é primo
    if (ehPrimo(numero)) {
        std::cout << "O número " << numero << " é primo!" << std::endl;
    } else {
        std::cout << "O número " << numero << " não é primo!" << std::endl;
    }

    std::string texto = "Olá, mundo!";

    // Inverte a string
    std::string textoInvertido = inverterString(texto);
    std::cout << "Texto original: " << texto << std::endl;
    std::cout << "Texto invertido: " << textoInvertido << std::endl;

    // Realiza operações matemáticas
    int resultado = (numero * 2) + pow(numero, 2);
    std::cout << "Resultado: " << resultado << std::endl;

    return 0;
}
```

Neste código, você encontrará duas funções: `ehPrimo`, que verifica se um número é primo, e `inverterString`, que inverte uma string. A função `main` é onde ocorre a execução principal do programa.

No exemplo, definimos um número `numero` e verificamos se ele é primo utilizando a função `ehPrimo`. Em seguida, temos uma string `texto`, que é invertida utilizando a função `inverterString`. Por fim, realizamos algumas operações matemáticas com o número `numero` e imprimimos o resultado.

Espero que este código complexo seja útil para você!