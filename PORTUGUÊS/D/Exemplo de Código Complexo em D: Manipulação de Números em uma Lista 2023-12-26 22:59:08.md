Claro! Aqui está um exemplo de código complexo em D:

```d
import std.stdio;

void main()
{
    writeln("Bem-vindo ao programa complexo em D!");

    // Definindo uma lista de números
    int[] numeros = [1, 2, 3, 4, 5];

    // Imprimindo os números em ordem crescente
    writeln("Números em ordem crescente:");
    for (int i = 0; i < numeros.length; i++)
    {
        writeln(numeros[i]);
    }

    // Imprimindo os números em ordem decrescente
    writeln("Números em ordem decrescente:");
    for (int i = numeros.length - 1; i >= 0; i--)
    {
        writeln(numeros[i]);
    }

    // Calculando a soma dos números
    int soma = 0;
    foreach (int numero; numeros)
    {
        soma += numero;
    }
    writeln("A soma dos números é: ", soma);

    // Calculando a média dos números
    double media = soma / numeros.length;
    writeln("A média dos números é: ", media);

    // Verificando se um número está presente na lista
    int numeroProcurado = 3;
    bool encontrado = false;
    foreach (int numero; numeros)
    {
        if (numero == numeroProcurado)
        {
            encontrado = true;
            break;
        }
    }
    if (encontrado)
    {
        writeln("O número ", numeroProcurado, " foi encontrado na lista.");
    }
    else
    {
        writeln("O número ", numeroProcurado, " não foi encontrado na lista.");
    }

    writeln("Fim do programa complexo em D!");
}
```

Este código em D realiza as seguintes tarefas:

1. Cria uma lista de números;
2. Imprime os números em ordem crescente;
3. Imprime os números em ordem decrescente;
4. Calcula a soma dos números;
5. Calcula a média dos números;
6. Verifica se um número específico está presente na lista.

Cada etapa é explicada através de comentários no código. Esta implementação mostra algumas características da linguagem D, como a sintaxe de loops `for` e `foreach`, o uso de arrays e a saída de texto usando a função `writeln`.

Lembrando que este é apenas um exemplo de código complexo em D e pode ser adaptado ou expandido de acordo com as necessidades do projeto.