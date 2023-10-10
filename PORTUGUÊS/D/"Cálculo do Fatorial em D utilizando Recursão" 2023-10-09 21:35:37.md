Claro! Aqui está um exemplo de código complexo em D que utiliza a recursão para calcular o fatorial de um número:

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

    writeln("Digite um número inteiro positivo:");
    readf("%lu", &numero);

    if (numero < 0) {
        writeln("Erro: O número deve ser positivo.");
        return;
    }

    writeln("O fatorial de ", numero, " é ", fatorial(numero));
}
```

Este código começa importando o módulo `std.stdio`, que é responsável por fornecer funções para entrada e saída padrão.

Em seguida, temos a função `fatorial`, que recebe um número `n` como argumento e retorna o fatorial desse número. O fatorial de `n` é calculado usando recursão: se `n` for igual a 0 ou 1, o fatorial é 1; caso contrário, o fatorial é calculado multiplicando `n` pelo fatorial de `n - 1`.

Na função `main`, é solicitado ao usuário que digite um número inteiro positivo. Em seguida, é feita uma verificação para garantir que o número seja positivo. Se o número for negativo, uma mensagem de erro é exibida e o programa é encerrado.

Por fim, o programa exibe o resultado do cálculo do fatorial do número digitado pelo usuário.

Espero que este exemplo tenha sido útil para você! Se tiver mais alguma dúvida ou precisar de mais exemplos, é só me perguntar.