Claro! Abaixo está um código complexo em linguagem D que é diferenciado e contém explicações detalhadas:

```d
import std.stdio;

void main()
{
    writeln("Olá, Mundo!"); // Imprime "Olá, Mundo!" na tela
    
    int[] numeros = [1, 2, 3, 4, 5]; // Cria um array de inteiros com os valores 1, 2, 3, 4 e 5
    
    foreach (num; numeros)
    {
        if (num % 2 == 0) // Verifica se o número é par
        {
            writeln(num, " é par"); // Imprime o número seguido de "é par"
        }
        else
        {
            writeln(num, " é ímpar"); // Imprime o número seguido de "é ímpar"
        }
    }
    
    int fatorial = calcularFatorial(5); // Calcula o fatorial de 5
    
    writeln("O fatorial de 5 é ", fatorial); // Imprime o resultado
    
    int resultado = somarNumeros(10, 20, 30); // Soma os números 10, 20 e 30
    
    writeln("A soma dos números é ", resultado); // Imprime o resultado
}

int calcularFatorial(int numero)
{
    if (numero <= 1)
    {
        return 1;
    }
    else
    {
        return numero * calcularFatorial(numero - 1); // Chama recursivamente a função até chegar a 1
    }
}

int somarNumeros(int a, int b, int c)
{
    return a + b + c;
}
```

Este código em D começa imprimindo a mensagem "Olá, Mundo!" na tela usando a função `writeln`. Em seguida, cria-se um array de inteiros chamado `numeros` com os valores 1, 2, 3, 4 e 5.

Em seguida, utiliza-se um laço `foreach` para iterar sobre cada número do array `numeros`. Dentro do laço, verifica-se se o número é par utilizando o operador de módulo `%`. Se o resto da divisão por 2 for igual a 0, significa que o número é par e imprime-se o número seguido de "é par". Caso contrário, imprime-se o número seguido de "é ímpar".

Após isso, há uma função chamada `calcularFatorial` que recebe um número como parâmetro e retorna o fatorial desse número. A função utiliza recursão para calcular o fatorial, multiplicando o número pelo fatorial do número anterior até chegar a 1.

Por fim, há uma função chamada `somarNumeros` que recebe três números inteiros como parâmetros e retorna a soma desses números.

No `main`, é chamada a função `calcularFatorial` passando o número 5 como argumento e o resultado é armazenado na variável `fatorial`. Em seguida, imprime-se o resultado.

Depois, é chamada a função `somarNumeros` passando os números 10, 20 e 30 como argumentos e o resultado é armazenado na variável `resultado`. Por fim, imprime-se o resultado.

Este código é apenas um exemplo complexo em linguagem D, mas pode ser adaptado e modificado de acordo com as necessidades do desenvolvedor.