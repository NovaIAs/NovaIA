```python
# Programa simplificado em Python para calcular o fatorial de um número natural

# Solicita ao usuário que insira um número natural
numero = int(input("Digite um número natural: "))

# Define uma função recursiva para calcular o fatorial do número
def fatorial_natural(numero):
    """
    Calcula o fatorial de um número natural usando a definição recursiva.

    Args:
        numero: O número natural cujo fatorial deve ser calculado.

    Returns:
        O fatorial do número natural fornecido.
    """

    # Caso base: Se o número for igual a 0, o fatorial é 1
    if numero == 0:
        return 1

    # Caso recursivo: Se o número for maior que 0, o fatorial é calculado
    # multiplicando o número pelo fatorial do número anterior
    else:
        return numero * fatorial_natural(numero - 1)

# Calcula o fatorial do número usando a função recursiva
fatorial = fatorial_natural(numero)

# Imprime o resultado do cálculo
print("O fatorial de {} é {}".format(numero, fatorial))
```

**Explicação do código:**

* A primeira linha do código solicita ao usuário que insira um número natural.
* A segunda linha define uma função recursiva chamada `fatorial_natural`, que recebe um parâmetro `numero` e retorna o fatorial desse parâmetro.
* A função `fatorial_natural` tem dois casos:
    * **Caso base:** Se o número for igual a 0, o fatorial é 1.
    * **Caso recursivo:** Se o número for maior que 0, o fatorial é calculado multiplicando o número pelo fatorial do número anterior.
* A décima primeira linha do código chama a função `fatorial_natural` com o parâmetro `numero` e armazena o resultado do cálculo na variável `fatorial`.
* A décima segunda linha do código imprime o resultado do cálculo do fatorial.