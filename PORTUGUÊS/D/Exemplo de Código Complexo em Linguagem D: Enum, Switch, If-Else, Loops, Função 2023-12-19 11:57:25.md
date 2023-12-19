Claro! Aqui está um exemplo de um código complexo em linguagem D:

```d
import std.stdio;

enum PESSOA {
    Joao,
    Maria,
    Jose
}

void main() {
    PESSOA pessoa = PESSOA.Joao;

    switch (pessoa) {
        case PESSOA.Joao:
            writeln("Olá, meu nome é João!");
            break;
        case PESSOA.Maria:
            writeln("Olá, meu nome é Maria!");
            break;
        case PESSOA.Jose:
            writeln("Olá, meu nome é José!");
            break;
        default:
            writeln("Olá, sou uma pessoa desconhecida!");
            break;
    }

    int idade = 30;
    if (idade >= 18) {
        writeln("Sou maior de idade!");
    } else {
        writeln("Sou menor de idade!");
    }

    for (int i = 0; i < 5; i++) {
        writeln("Contagem regressiva: ", 5 - i);
    }

    int[] numeros = [1, 2, 3, 4, 5];
    foreach (num in numeros) {
        writeln("Número: ", num);
    }

    int soma = somar(10, 20);
    writeln("A soma de 10 e 20 é ", soma);
}

int somar(int num1, int num2) {
    return num1 + num2;
}
```

Neste código em D, temos uma enumeração `PESSOA` que representa três pessoas diferentes: João, Maria e José. O programa então usa um switch para imprimir uma mensagem dependendo da pessoa selecionada.

Em seguida, temos uma verificação de idade usando um if-else. Se a idade for maior ou igual a 18, o programa imprime "Sou maior de idade!", caso contrário, imprime "Sou menor de idade!".

O código também inclui um loop for que faz uma contagem regressiva de 5 até 1, e um loop foreach que itera sobre um array de números e imprime cada número.

Por fim, há uma função `somar` que recebe dois números como argumentos e retorna sua soma. O código principal chama essa função e imprime o resultado.

Esse é apenas um exemplo de código complexo em D, e pode ser adaptado ou expandido de acordo com as necessidades específicas do projeto.