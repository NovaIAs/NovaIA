Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>

using namespace std;

// Função auxiliar para verificar se um número é primo
bool ehPrimo(int num) {
    if (num <= 1) {
        return false;
    }

    for (int i = 2; i * i <= num; i++) {
        if (num % i == 0) {
            return false;
        }
    }

    return true;
}

// Função principal
int main() {
    int n;
    cout << "Digite um número inteiro positivo: ";
    cin >> n;

    vector<int> primos;
    int i = 2;
    while (primos.size() < n) {
        if (ehPrimo(i)) {
            primos.push_back(i);
        }
        i++;
    }

    cout << "Os " << n << " primeiros números primos são: ";
    for (int j = 0; j < primos.size(); j++) {
        cout << primos[j] << " ";
    }
    cout << endl;

    return 0;
}
```

Neste código, primeiro importamos as bibliotecas `iostream` e `vector`. Em seguida, definimos uma função `ehPrimo` que verifica se um número é primo ou não. Essa função recebe um inteiro como parâmetro e retorna um valor booleano indicando se o número é primo ou não.

Na função principal `main`, o usuário é solicitado a digitar um número inteiro positivo. Em seguida, criamos um vetor `primos` que armazenará os números primos encontrados.

Em um loop `while`, iniciamos uma variável `i` com o valor 2 e continuamos a incrementá-la até que o vetor `primos` tenha o tamanho desejado, ou seja, `n`. Em cada iteração, verificamos se o número `i` é primo utilizando a função `ehPrimo`. Se for, adicionamos o número ao vetor `primos` usando a função `push_back`. Por fim, incrementamos `i`.

Após sair do loop, exibimos os `n` primeiros números primos encontrados utilizando um loop `for` e a função `cout`. Por fim, retornamos 0 para indicar que o programa foi executado com sucesso.

Espero que este código atenda às suas expectativas!