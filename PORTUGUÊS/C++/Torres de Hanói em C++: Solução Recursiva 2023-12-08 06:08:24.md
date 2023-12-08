Claro! Farei um código complexo em C++ que resolve o problema das Torres de Hanói de forma recursiva. As Torres de Hanói é um famoso quebra-cabeças matemático, no qual temos três pilhas onde, inicialmente, todas as peças estão dispostas em uma das pilhas. O objetivo é mover todas as peças para outra pilha, seguindo as seguintes regras:

1. Apenas uma peça pode ser movida por vez.
2. Uma peça maior nunca pode ser colocada sobre uma peça menor.

O código em C++ a seguir resolverá o problema das Torres de Hanói utilizando recursão:

```cpp
#include <iostream>
using namespace std;

void moverTorre(int n, char origem, char destino, char auxiliar) {
    if (n == 1) {
        cout << "Mover disco 1 da torre " << origem << " para a torre " << destino << endl;
    } else {
        moverTorre(n-1, origem, auxiliar, destino);
        cout << "Mover disco " << n << " da torre " << origem << " para a torre " << destino << endl;
        moverTorre(n-1, auxiliar, destino, origem);
    }
}

int main() {
    int numDiscos;

    cout << "Informe o número de discos: ";
    cin >> numDiscos;

    moverTorre(numDiscos, 'A', 'C', 'B');

    return 0;
}
```

Explicação do código:

1. A função `moverTorre` é responsável por resolver o problema das Torres de Hanói de forma recursiva. Ela recebe quatro parâmetros: o número de discos `n`, a torre de origem `origem`, a torre de destino `destino` e a torre auxiliar `auxiliar`.
2. Na linha 6, verificamos se há apenas um disco para ser movido. Nesse caso, imprimimos a mensagem indicando a movimentação direta da torre origem para a torre destino.
3. Caso contrário, chamamos a função `moverTorre` recursivamente, dividindo o problema em subproblemas menores.
4. Na linha 9, movemos n-1 discos da torre origem para a torre auxiliar, utilizando a torre destino como auxiliar.
5. Na linha 10, movemos o disco restante (o maior) da torre origem para a torre destino.
6. Na linha 11, movemos os n-1 discos que estavam na torre auxiliar para a torre destino, utilizando a torre origem como auxiliar.
7. No programa principal, na função `main`, solicitamos ao usuário o número de discos a serem movidos.
8. Chamamos a função `moverTorre` com os valores informados pelo usuário.
9. Após a execução, o programa exibirá as instruções detalhadas para resolver o problema das Torres de Hanói.

Espero que este código complexo em C++ atenda às suas expectativas!