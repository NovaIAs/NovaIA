Claro! Vou criar um código em Python que implementa um algoritmo de busca binária recursiva.

A busca binária é um algoritmo eficiente para buscar elementos em uma lista ordenada. Ele divide repetidamente a lista em duas metades e verifica se o elemento desejado está na metade esquerda ou direita. Esse processo é repetido até que o elemento seja encontrado ou a lista seja reduzida a um único elemento.

Aqui está o código em Python que implementa a busca binária recursiva:

```python
def busca_binaria_recursiva(lista, elemento, inicio=0, fim=None):
    if fim is None:
        fim = len(lista) - 1

    if inicio > fim:
        return -1

    meio = (inicio + fim) // 2

    if lista[meio] == elemento:
        return meio
    elif lista[meio] > elemento:
        return busca_binaria_recursiva(lista, elemento, inicio, meio - 1)
    else:
        return busca_binaria_recursiva(lista, elemento, meio + 1, fim)
```

Explicação do código:
- A função `busca_binaria_recursiva` recebe quatro parâmetros: a lista onde o elemento será buscado, o elemento que estamos procurando, o índice inicial da sublista (opcional, padrão é 0) e o índice final da sublista (opcional, padrão é o último índice da lista).
- Primeiro, verificamos se o índice inicial é maior que o índice final. Se for, significa que a lista foi reduzida a um único elemento e o elemento não foi encontrado, então retornamos -1.
- Em seguida, calculamos o índice do elemento do meio da sublista utilizando a fórmula `(inicio + fim) // 2`. Usamos a divisão inteira para garantir que o resultado seja um número inteiro.
- Comparamos o elemento do meio com o elemento desejado. Se forem iguais, encontramos o elemento e retornamos o seu índice.
- Se o elemento do meio for maior que o elemento desejado, chamamos a função recursivamente passando a sublista da esquerda (do início até o elemento do meio - 1).
- Se o elemento do meio for menor que o elemento desejado, chamamos a função recursivamente passando a sublista da direita (do elemento do meio + 1 até o fim).
- A busca binária recursiva continua dividindo a lista em duas metades até encontrar o elemento desejado ou até que a lista seja reduzida a um único elemento.

Espero que esse código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição.