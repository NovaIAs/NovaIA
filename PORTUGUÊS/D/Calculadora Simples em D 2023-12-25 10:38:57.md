Claro! Aqui está um exemplo de código complexo em D, que cria uma calculadora simples com operações básicas:

```d
import std.stdio;
import std.conv;
import std.algorithm;

void main()
{
    writeln("Bem-vindo à calculadora!");

    while (true)
    {
        writeln("\nEscolha uma operação:");
        writeln("1. Soma");
        writeln("2. Subtração");
        writeln("3. Multiplicação");
        writeln("4. Divisão");
        writeln("5. Sair");

        char[] input = readln().strip().array;
        int choice = to!int(input);

        if (choice == 5)
        {
            writeln("Obrigado por usar a calculadora!");
            break;
        }

        writeln("Digite o primeiro número:");
        int num1 = to!int(readln().strip());

        writeln("Digite o segundo número:");
        int num2 = to!int(readln().strip());

        switch (choice)
        {
            case 1:
                writeln("Resultado: ", num1 + num2);
                break;
            case 2:
                writeln("Resultado: ", num1 - num2);
                break;
            case 3:
                writeln("Resultado: ", num1 * num2);
                break;
            case 4:
                if (num2 != 0)
                    writeln("Resultado: ", num1 / num2);
                else
                    writeln("Erro: divisão por zero!");
                break;
            default:
                writeln("Opção inválida!");
                break;
        }
    }
}
```

Explicação do código:

1. O código começa importando as bibliotecas `std.stdio`, `std.conv` e `std.algorithm` para utilizar as funções de entrada e saída, conversão de tipos e manipulação de strings.
2. A função `main()` é o ponto de entrada do programa.
3. O programa exibe uma mensagem de boas-vindas e entra em um loop infinito.
4. Dentro do loop, o usuário pode escolher uma operação digitando o número correspondente.
5. O código lê a escolha do usuário e converte-a para um número inteiro.
6. Se o usuário escolher a opção 5, o programa exibe uma mensagem de agradecimento e sai do loop, encerrando o programa.
7. Se o usuário escolher uma operação válida (1 a 4), o programa solicita que o usuário digite dois números.
8. Os números digitados são convertidos para inteiros.
9. Dependendo da operação escolhida, o programa realiza a operação apropriada e exibe o resultado.
10. Se a escolha for uma divisão e o segundo número for zero, o programa exibe uma mensagem de erro.
11. Se a escolha não for válida, o programa exibe uma mensagem de opção inválida.
12. O loop continua até que o usuário escolha sair do programa.

Espero que isso atenda às suas expectativas! Se você tiver alguma outra solicitação, por favor, me avise.