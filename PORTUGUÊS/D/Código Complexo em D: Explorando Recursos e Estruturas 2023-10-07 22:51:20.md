Certamente! Aqui está um exemplo de um código complexo em D:

```d
import std.stdio;

void main()
{
    writeln("Bem-vindo ao programa complexo em D!");

    // Declaração de variáveis
    int numero = 10;
    float decimal = 3.14;
    bool condicao = true;
    string texto = "Olá, mundo!";

    // Estruturas de controle
    if (numero > 5)
    {
        writeln("O número é maior que 5!");
    }
    else
    {
        writeln("O número é menor ou igual a 5!");
    }

    for (int i = 0; i < 10; i++)
    {
        writeln("Iteração ", i);
    }

    while (condicao)
    {
        writeln("Loop infinito!");
        condicao = false;
    }

    // Funções
    int soma = somar(5, 7);
    writeln("A soma é ", soma);

    // Classes e objetos
    Pessoa pessoa = new Pessoa("João", 25);
    pessoa.apresentar();

    // Arrays
    int[] numeros = [1, 2, 3, 4, 5];
    foreach (int num; numeros)
    {
        writeln(num);
    }

    // Manipulação de strings
    string maiusculas = texto.toUpper();
    writeln("Texto em maiúsculas: ", maiusculas);

    // Outros recursos
    writeln("Pi: ", Math.PI);
    writeln("Aleatório: ", Math.random());

    writeln("Fim do programa complexo em D!");
}

int somar(int a, int b)
{
    return a + b;
}

class Pessoa
{
    string nome;
    int idade;

    this(string nome, int idade)
    {
        this.nome = nome;
        this.idade = idade;
    }

    void apresentar()
    {
        writeln("Olá, meu nome é ", nome, " e eu tenho ", idade, " anos.");
    }
}
```

Neste código, demonstro diversos recursos da linguagem D. Começo com a declaração de variáveis de diferentes tipos, como int, float, bool e string. Em seguida, utilizo estruturas de controle, como if, else, for e while, para tomar decisões e repetir ações. 

A seguir, defino uma função `somar` que recebe dois números inteiros e retorna a soma deles. Em seguida, crio uma classe `Pessoa` com propriedades nome e idade, um construtor e um método `apresentar` que exibe uma mensagem com essas informações.

Também mostro como utilizar arrays em D, percorrendo-os com um loop `foreach`. Além disso, aplico operações de manipulação de strings, como converter um texto para maiúsculas.

Por fim, utilizo recursos adicionais da linguagem D, como as constantes `Math.PI` e `Math.random()` para obter o valor de pi e gerar um número aleatório.

Espero que esse código complexo em D atenda às suas expectativas e demonstre a variedade de recursos disponíveis nesta linguagem de programação!