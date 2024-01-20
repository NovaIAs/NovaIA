```portuguol
programa números_primos
{
    # Utiliza a declaração de função para construir uma rotina que recebe um valor inteiro positivo e retorna um Booleano indicando se esse valor é primo.
    função primo(n: inteiro): booleano
    {
        se n <= 1
        {
            retorna falso
        }

        para i de 2 até n div 2
        {
            se n mod i = 0
            {
                retorna falso
            }
        }

        retorna verdadeiro
    }

    # Esta variável irá armazenar a lista de números primos gerados.
    var primos: vetor[1..100] de inteiro

    # O contador irá rastrear o número de números primos encontrados.
    var contador: inteiro

    # Inicializa o contador com 0.
    contador := 0

    # Inicia um loop para encontrar os primeiros 100 números primos.
    para n de 2 até 100
    {
        # Verifica se o número atual é primo, usando a função "primo".
        se primo(n)
        {
            # Se o número atual for primo, incrementa o contador e adiciona o número à lista de números primos.
            contador += 1
            primos[contador] := n
        }
    }

    # Exibe a lista de números primos gerada.
    para i de 1 até contador
    {
        escreva(primos[i], ' ')
    }

    # Exibe uma mensagem informando o número total de números primos encontrados.
    escreva('Número total de números primos encontrados:', contador)
}
```

### Explicação do código:

* O código começa definindo uma função chamada `primo`. Esta função recebe um inteiro positivo `n` como argumento e retorna `verdadeiro` se `n` for um número primo, e `falso` caso contrário.
* A função `primo` verifica se `n` é menor ou igual a 1. Se for, a função retorna `falso`, pois números menores ou iguais a 1 não são primos.
* Se `n` for maior que 1, a função inicia um loop que vai de 2 até `n` dividido por 2. Este loop verifica se `n` é divisível por qualquer número entre 2 e `n` dividido por 2.
* Se `n` for divisível por algum número entre 2 e `n` dividido por 2, a função retorna `falso`, pois `n` não é primo.
* Se `n` não for divisível por nenhum número entre 2 e `n` dividido por 2, a função retorna `verdadeiro`, pois `n` é um número primo.
* O programa principal declara uma variável `primos` como um vetor de inteiros de tamanho 100. Esta variável será usada para armazenar os primeiros 100 números primos encontrados.
* O programa também declara uma variável `contador` como um inteiro. Esta variável será usada para rastrear o número de números primos encontrados.
* O contador é inicializado com 0.
* O programa inicia um loop que vai de 2 a 100. Este loop é usado para encontrar os primeiros 100 números primos.
* Para cada número `n` no loop, o programa chama a função `primo` para verificar se `n` é um número primo.
* Se a função `primo` retornar `verdadeiro`, o programa incrementa o contador e adiciona `n` à lista de números primos.
* O programa exibe a lista de números primos gerada.
* Por fim, o programa exibe uma mensagem informando o número total de números primos encontrados.