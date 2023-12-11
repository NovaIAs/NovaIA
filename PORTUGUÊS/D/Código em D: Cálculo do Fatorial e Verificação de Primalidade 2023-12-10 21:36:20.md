Certamente! Aqui está um código complexo em D, com comentários explicativos em português:

```d
import std.stdio;

// Função para calcular o fatorial de um número usando recursão
ulong fatorial(ulong num)
{
    if (num <= 1)
        return 1;
    else
        return num * fatorial(num - 1);
}

// Função para verificar se um número é primo
bool ehPrimo(ulong num)
{
    if (num <= 1)
        return false;

    for (ulong i = 2; i * i <= num; i++)
    {
        if (num % i == 0)
            return false;
    }

    return true;
}

// Função principal
void main()
{
    ulong numero = 10;
    ulong resultadoFatorial = fatorial(numero);
    bool resultadoPrimalidade = ehPrimo(numero);

    writeln("O fatorial de ", numero, " é: ", resultadoFatorial);

    if (resultadoPrimalidade)
        writeln(numero, " é um número primo.");
    else
        writeln(numero, " não é um número primo.");
}
```

Neste código em D, temos uma função chamada `fatorial` que calcula o fatorial de um número utilizando recursão. A função `ehPrimo` verifica se um número é primo ou não, utilizando um loop de verificação. A função `main` é a função principal, onde é definido um número (neste caso, 10) e chamamos as funções `fatorial` e `ehPrimo` para calcular o fatorial desse número e verificar sua primalidade.

O resultado do fatorial é exibido na tela, assim como uma mensagem informando se o número é primo ou não.