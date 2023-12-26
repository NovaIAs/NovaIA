Claro! Aqui está um exemplo de um código complexo em D que utiliza recursão para calcular o fatorial de um número:

```d
import std.stdio;

ulong fatorial(ulong n)
{
    if (n == 0 || n == 1)
        return 1;
    else
        return n * fatorial(n - 1);
}

void main()
{
    ulong numero;
    writeln("Digite um número inteiro não negativo:");
    readf("%lu", &numero);
    
    writeln("O fatorial de ", numero, " é ", fatorial(numero));
}
```

Neste código, temos uma função `fatorial` que calcula o fatorial de um número utilizando recursão. A função verifica se o número é igual a 0 ou 1 e, caso seja, retorna 1. Caso contrário, ela chama a si mesma, passando como parâmetro o número decrementado em 1, e multiplica o resultado pela própria chamada recursiva.

No `main`, o código solicita ao usuário que digite um número inteiro não negativo. Em seguida, utiliza a função `readf` para ler o número digitado e armazená-lo na variável `numero`. Por fim, imprime na tela o resultado do cálculo do fatorial utilizando a função `writeln`.

Espero que este código atenda às suas expectativas! Se tiver mais alguma solicitação, é só me informar.