Claro! Aqui está um código complexo em Python:

```python
# Bibliotecas necessárias
import math

# Classe Pessoa
class Pessoa:
    def __init__(self, nome, idade):
        self.nome = nome
        self.idade = idade

# Função para calcular o fatorial
def fatorial(numero):
    if numero == 0:
        return 1
    else:
        return numero * fatorial(numero - 1)

# Função para verificar se um número é primo
def is_primo(numero):
    if numero < 2:
        return False
    for i in range(2, int(math.sqrt(numero)) + 1):
        if numero % i == 0:
            return False
    return True

# Lista de pessoas
pessoas = [
    Pessoa("João", 25),
    Pessoa("Maria", 30),
    Pessoa("Pedro", 40),
    Pessoa("Ana", 20)
]

# Imprime o fatorial de cada idade das pessoas
for pessoa in pessoas:
    fatorial_idade = fatorial(pessoa.idade)
    print(f"O fatorial da idade de {pessoa.nome} é {fatorial_idade}")

# Imprime todos os números primos menores que 1000
primos = []
for i in range(2, 1000):
    if is_primo(i):
        primos.append(i)
print("Números primos menores que 1000:", primos)
```

Neste código, temos uma classe `Pessoa` que possui atributos `nome` e `idade`. Em seguida, temos a função `fatorial` que calcula o fatorial de um número utilizando recursão. A função `is_primo` verifica se um número é primo, utilizando um loop e a função `sqrt` da biblioteca `math`.

Em seguida, temos uma lista de pessoas com diferentes nomes e idades. O código percorre essa lista e calcula o fatorial da idade de cada pessoa, imprimindo o resultado.

Por fim, o código gera uma lista de todos os números primos menores que 1000, utilizando a função `is_primo`, e imprime essa lista.

Espero que esse código atenda às suas expectativas!