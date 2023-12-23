Claro! Aqui está um código complexo em D que utiliza recursão para calcular o fatorial de um número:

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
    writeln("Digite um número para calcular o fatorial: ");
    readln(numero);
    
    ulong resultado = fatorial(numero);
    
    writeln("O fatorial de ", numero, " é: ", resultado);
}
```
Explicação do código:

1. A função `fatorial` é definida, recebendo um parâmetro do tipo `ulong` (número inteiro não negativo).
2. A função verifica se o valor de `n` é igual a 0 ou 1. Se for, retorna 1, pois o fatorial de 0 e 1 é sempre igual a 1.
3. Caso contrário, a função chama a si mesma, passando `n - 1` como argumento, e multiplica o resultado pelo valor de `n`. Esse processo se repete até que `n` seja igual a 0 ou 1, quando a recursão termina.
4. A função `main` é definida como o ponto de entrada do programa.
5. É declarada uma variável `numero` do tipo `ulong` para armazenar o número fornecido pelo usuário.
6. O usuário é solicitado a digitar um número para calcular o fatorial.
7. O valor digitado pelo usuário é lido e armazenado na variável `numero`.
8. A função `fatorial` é chamada, passando o valor de `numero` como argumento. O resultado é armazenado na variável `resultado`.
9. O programa exibe na tela o resultado do cálculo do fatorial.