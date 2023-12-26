Claro! Vou gerar um código complexo na linguagem D para você. Aqui está um exemplo de um programa que utiliza recursão para calcular o fatorial de um número:

```d
import std.stdio;

ulong fatorial(ulong n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * fatorial(n - 1);
    }
}

void main() {
    ulong numero;
    write("Digite um número inteiro positivo: ");
    readf("%lu", &numero);
    writeln("O fatorial de ", numero, " é ", fatorial(numero));
}
```

Neste código, temos a função `fatorial`, que recebe um número inteiro positivo `n` e retorna o seu fatorial. A função utiliza recursão para calcular o fatorial, ou seja, ela chama a si mesma passando `n - 1` como argumento até chegar a um caso base onde `n` é igual a 0 ou 1. Nesses casos, a função retorna 1.

No `main`, o usuário é solicitado a digitar um número inteiro positivo e o valor é lido através da função `readf`. Em seguida, o programa chama a função `fatorial` passando o número digitado como argumento e exibe o resultado na tela utilizando a função `writeln`.

Espero que esse código atenda às suas expectativas!